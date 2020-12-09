#MDSF 08/26/2020
# (original code taken from
# https://medium.com/@pallawi.ds/semantic-segmentation-with-u-net-train-and-test-on-your-custom-data-in-keras-39e4f972ec89)
# and (https://segmentation-models.readthedocs.io/en/latest/api.html)

import os, sys, csv, argparse, time, datetime, random, glob, warnings, cv2
import numpy as np
import pandas as pd
from PIL import Image
from tqdm import tqdm
from itertools import chain
from skimage.io import imread, imshow, imread_collection, concatenate_images
from skimage.transform import resize
from skimage.morphology import label
from skimage.color import rgb2gray
from keras.models import Model, load_model, Sequential
from keras.layers import Input, Dense, Activation, Flatten, Dropout
from keras import optimizers, losses, metrics
from keras.optimizers import SGD, Adam
from keras.layers.core import Dropout, Lambda
from keras.layers.convolutional import Conv2D, Conv2DTranspose
from keras.layers.pooling import MaxPooling2D
from keras.layers.merge import concatenate
from keras.callbacks import EarlyStopping, ModelCheckpoint, LearningRateScheduler, TensorBoard
from keras import backend as K
from keras.applications.vgg16 import VGG16
import tensorflow as tf
import segmentation_models
from segmentation_models import Unet
from segmentation_models.utils import set_trainable
import tensorboard
!rm -rf ./logs/

ControlFile = "/Users/mayasamuels-fair/Desktop/Segmentation/TPSSegmentationSettings.txt"

def str_to_bool(s):
    if s == 'True':
        return True
    elif s == 'False':
        return False
    else:
        raise ValueError

if not ControlFile is None:
    Args = np.genfromtxt(fname=ControlFile, comments='#', missing_values='None', filling_values=None, autostrip=False,
                         dtype=(str))
    IMG_WIDTH = int(Args[5][2])
    WorkingDir = Args[1][2]
    TRAIN_PATH = Args[12][2]
    TEST_PATH = Args[13][2]
    model_name = Args[14][2]
    SET_SEED = str_to_bool(Args[15][2])
    backbone = Args[16][2]
    patience = float(Args[17][2])
    val_split = float(Args[18][2])
    batch = float(Args[19][2])
    epochs = float(Args[20][2])

# Parameters: remember the image has to be square
parser = argparse.ArgumentParser(description="This script trains the model")
parser.add_argument('--IMG_WIDTH', action="store", dest="IMG_WIDTH", type=int)
parser.add_argument('--TRAIN_PATH', action="store", dest="TRAIN_PATH")
parser.add_argument('--TEST_PATH', action="store", dest="TEST_PATH")
parser.add_argument('--WORKING_DIR', action="store", dest="WorkingDir")
parser.add_argument('--MODEL', action="store", dest="model_name", default="transfer_model.h5")
parser.add_argument('--SET_SEED', action="store", dest="SET_SEED", type=bool, default=False)
parser.add_argument('--BACKBONE', action="store", dest="backbone", default='vgg16')
parser.add_argument('--PATIENCE', action="store", dest="patience", type=int, default=20)
parser.add_argument('--VAL_SPLIT', action="store", dest="val_split", default=0.2)
parser.add_argument('--BATCH_SIZE', action="store", dest="batch", type=int, default=40)
parser.add_argument('--EPOCHS', action="store", dest="epochs", type=int, default=100)
args = parser.parse_known_args()

IMG_WIDTH = args[0].IMG_WIDTH
TRAIN_PATH = args[0].TRAIN_PATH
TEST_PATH = args[0].TEST_PATH
WORKING_DIR = args[0].WorkingDir
model_name = args[0].model_name
SET_SEED = args[0].SET_SEED
backbone = args[0].backbone
patience = args[0].patience
val_split = args[0].val_split
batch = args[0].batch
epochs = args[0].epochs

# Choice to make model replicable
if (SET_SEED==True):
    parser.add_argument('--SET_SEED_TO', action="store", dest="SEED", type=int, default=365)
    random.seed = SEED
    np.random.seed = SEED
args = parser.parse_known_args()

#Option to set parameters within script if not using terminal
if (IMG_WIDTH == None):
    IMG_WIDTH = 256
    TEST_PATH = "/Users/mayasamuels-fair/Desktop/WorkingFolder/Test"
    TRAIN_PATH = "/Users/mayasamuels-fair/Desktop/WorkingFolder/Training"
    WorkingDir = "/Users/mayasamuels-fair/Desktop/WorkingFolder"
    model_name = "transfer_model.h5"
    backbone = 'vgg16'
    batch = 40
    epochs = 100
    patience = 20
    val_split = 0.2

IMG_HEIGHT = IMG_WIDTH
IMG_CHANNELS = 3
warnings.filterwarnings('ignore', category=UserWarning, module='skimage')
print("Imported all the dependencies and parameters")

# Get train and test IDs
train_ids = next(os.walk(TRAIN_PATH))[1]
test_ids = next(os.walk(TEST_PATH))[2][2:]

# Get and resize train images and masks
X_train = np.zeros((len(train_ids), IMG_HEIGHT, IMG_WIDTH, IMG_CHANNELS), dtype=np.uint8)
Y_train = np.zeros((len(train_ids), IMG_HEIGHT, IMG_WIDTH, 1), dtype=np.bool)

print("X_train",X_train.shape)
print("Y_train",Y_train.shape)
print('Getting and resizing train images and masks ... ')
sys.stdout.flush()

from PIL import ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True
for n, id_ in tqdm(enumerate(train_ids), total=len(train_ids)):
    path = TRAIN_PATH + '/' + id_
    img = imread(path + '/images/' + id_ + '.png')[:,:,:IMG_CHANNELS]
    img = resize(img, (IMG_HEIGHT, IMG_WIDTH), mode='constant', preserve_range=True)
    X_train[n] = img
    mask = np.zeros((IMG_HEIGHT, IMG_WIDTH, 1))
    for mask_file in next(os.walk(path + '/masks/'))[2]:
        if mask_file.endswith(".png"):
            mask_ = cv2.imread(path + '/masks/' + mask_file, 0)
            mask_ = np.expand_dims(resize(mask_, (IMG_HEIGHT, IMG_WIDTH), mode='constant', preserve_range=True), axis=-1)
            mask = np.maximum(mask, mask_)
    Y_train[n] = mask

# Get and resize test images
X_test = np.zeros((len(test_ids), IMG_HEIGHT, IMG_WIDTH, IMG_CHANNELS), dtype=np.uint8)
sizes_test = []
print('Getting and resizing test images ... ')
sys.stdout.flush()
from PIL import ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True
for n, id_ in tqdm(enumerate(test_ids), total=len(test_ids)):
    path = TEST_PATH + '/' + id_
    img = imread(path)[:,:,:IMG_CHANNELS]
    sizes_test.append([img.shape[0], img.shape[1]])
    img = resize(img, (IMG_HEIGHT, IMG_WIDTH), mode='constant', preserve_range=True)
    X_test[n] = img

print('Done importing images')

#Define IoU metric
def mean_iou(y_true, y_pred):
    prec = []
    for t in np.arange(0.5, 1.0, 0.05):
        y_pred_ = tf.to_int32(y_pred > t)
        score, up_opt = tf.metrics.mean_iou(y_true, y_pred_, 2)
        K.get_session().run(tf.local_variables_initializer())
        with tf.control_dependencies([up_opt]):
            score = tf.identity(score)
        prec.append(score)
    return K.mean(K.stack(prec), axis=0)

# Build U-Net model (transfer model version, must match model in transfer_test_loop.py)
logdir="logs/fit/" + datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
tensorboard = TensorBoard(log_dir=logdir)
transfer_model = Unet(backbone_name=backbone, input_shape=(None,None,3), classes=1,
             activation='relu', encoder_weights='imagenet', encoder_freeze=True)
transfer_model.compile(optimizer='Adam', loss='binary_crossentropy', metrics=[mean_iou])
transfer_model.summary()
earlystopper = EarlyStopping(patience=patience, verbose=1)
checkpointer = ModelCheckpoint((WorkingDir + '/' + model_name), verbose=1, save_best_only=True)
results = transfer_model.fit(X_train, Y_train, validation_split=val_split, batch_size=batch, epochs=epochs, callbacks=[earlystopper, checkpointer, tensorboard])

# Evaluate how well training went in tensorboard
%load_ext tensorboard
%tensorboard --logdir logs --host=127.0.0.1