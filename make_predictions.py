#MDSF 08/26/2020
# (original code taken from
# https://medium.com/@pallawi.ds/semantic-segmentation-with-u-net-train-and-test-on-your-custom-data-in-keras-39e4f972ec89)

import os, sys, random, warnings, cv2, glob, argparse
import numpy as np
import pandas as pd
from tqdm import tqdm
from itertools import chain
from skimage.io import imread, imshow, imread_collection, concatenate_images
from skimage.transform import resize
from skimage.morphology import label
from scipy import ndimage
from keras.models import Model, load_model
from keras.layers import Input
from keras import optimizers
from keras.layers.core import Dropout, Lambda
from keras.layers.convolutional import Conv2D, Conv2DTranspose
from keras.layers.pooling import MaxPooling2D
from keras.layers.merge import concatenate
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras import backend as K
import tensorflow as tf
from PIL import Image
import segmentation_models
from segmentation_models import Unet
from segmentation_models.utils import set_trainable
np.set_printoptions(threshold = sys.maxsize)

ControlFile = "/Users/mayasamuels-fair/Desktop/Segmentation/TPSSegmentationSettings.txt"

if not ControlFile = None:
    Args = np.genfromtxt(fname=ControlFile, comments='#', missing_values='None', filling_values=None, autostrip=False,
                         dtype=(str))
    TEST_FOLDER = Args[13][2]
    MODEL_PATH = Args[14][2]
    WORKING_DIR = Args[1][2]
    INIT_IMG_WIDTH = float(Args[5][2])
    FIN_IMG_WIDTH = float(Args[21][2])
    backbone = Args[16][2]

# Set parameters
parser = argparse.ArgumentParser(description="This script uses the model to produce a contour txt file for each specimen")
parser.add_argument('--TEST_FOLDER', action="store", dest="TEST_FOLDER")
parser.add_argument('--MODEL_PATH', action="store", dest="MODEL_PATH")
parser.add_argument('--WORKING_DIR', action="store", dest="WORKING_DIR")
parser.add_argument('--INIT_IMG_WIDTH', action="store", dest="INIT_IMG_WIDTH", type=int)
parser.add_argument('--FIN_IMG_WIDTH', action="store", dest="FIN_IMG_WIDTH", type=int)
parser.add_argument('--BACKBONE', action="store", dest="backbone", default='vgg16')
args = parser.parse_known_args()

TEST_FOLDER = args[0].TEST_FOLDER
MODEL_PATH = args[0].MODEL_PATH
WORKING_DIR = args[0].WORKING_DIR
INIT_IMG_WIDTH = args[0].INIT_IMG_WIDTH
FIN_IMG_WIDTH = args[0].FIN_IMG_WIDTH
backbone = args[0].backbone

if (TEST_FOLDER==None):
    TEST_FOLDER = "/Users/mayasamuels-fair/Desktop/WorkingFolder/Test"
    MODEL_PATH = "/Users/mayasamuels-fair/Desktop/WorkingFolder/transfer_model.h5"
    WORKING_DIR = "/Users/mayasamuels-fair/Desktop/WorkingFolder"
    INIT_IMG_WIDTH = 256
    FIN_IMG_WIDTH = 2880
    backbone = 'vgg16'

class automaticmaplabelling():
    def __init__(self,modelPath,full_chq,imagePath,width,height,channels):
        print (modelPath)
        print(imagePath)
        self.modelPath=modelPath
        self.full_chq=full_chq
        self.imagePath=imagePath
        self.IMG_WIDTH=width
        self.IMG_HEIGHT=height
        self.IMG_CHANNELS=channels
        self.transfer_model = self.U_net()

    def mean_iou(self, y_true, y_pred):
        prec = []
        for t in np.arange(0.5, 1.0, 0.05):
            y_pred_ = tf.to_int32(y_pred > t)
            score, up_opt = tf.metrics.mean_iou(y_true, y_pred, 2)
            K.get_session().run(tf.local_variables_initializer())
            with tf.control_dependencies([up_opt]):
                score = tf.identity(score)
            prec.append(score)
        return K.mean(K.stack(prec), axis=0)

    def U_net(self):
        # Build U-Net model
        transfer_model = Unet(backbone_name=backbone, input_shape=(None, None, 3), classes=1,
                              activation='relu', encoder_weights='imagenet', encoder_freeze=True)
        transfer_model.compile(optimizer='Adam', loss='binary_crossentropy', metrics=[self.mean_iou])
        transfer_model.load_weights(self.modelPath)
        transfer_model.summary()
        return transfer_model

    def prediction(self):
        img = cv2.imread(self.imagePath, 0)
        img = resize(img, (INIT_IMG_WIDTH, INIT_IMG_WIDTH), mode='constant', preserve_range=True)
        img = np.expand_dims(img, axis=-1)
        x_test = np.zeros((1, self.IMG_HEIGHT, self.IMG_WIDTH, self.IMG_CHANNELS), dtype=np.uint8)
        x_test[0] = img

        preds_test = self.transfer_model.predict(x_test, verbose=1)
        preds_test = preds_test / preds_test.max()
        preds_test = (preds_test > 0.5).astype(np.uint8)
        mask = preds_test[0]

        for i in range(mask.shape[0]): #this for loop makes the mask binary
            for j in range(mask.shape[1]):
                if mask[i][j] == 1:
                    mask[i][j] = 255
                else:
                    mask[i][j] = 0
        mask = cv2.resize(mask, (FIN_IMG_WIDTH, FIN_IMG_WIDTH))
        edges = cv2.Canny(mask, 0, 255)
        # edges = cv2.merge((edges, edges, edges))
        edges = ndimage.gaussian_filter(edges, sigma = 2) # blurs the mask for better edge detection
        contours, hierarchy = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        print(len(contours))
        contour = max(contours, key = len) #get the longest contour
        # contour = np.resize(contour, (FIN_IMG_WIDTH, FIN_IMG_WIDTH))
        coords = np.array2string(contour)
        open((WORKING_DIR + "/Contours/contour_{Name}.txt".format(Name = ID)), "w").write(coords)
        return x_test[0], mask

os.chdir(TEST_FOLDER)
if not os.path.exists((WorkingDirectory + "/Contours")):
    os.mkdir((WorkingDirectory + "/Contours"))
for file in glob.glob('*.png'):
    ID = file.split('.')[0]
    automaticmaplabellingobj = automaticmaplabelling(MODEL_PATH, True, file, INIT_IMG_WIDTH, INIT_IMG_WIDTH, 3)
    testimg,mask = automaticmaplabellingobj.prediction()