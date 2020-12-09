# MDSF 08/26/2020
# Here are several functions to prepare image and TPS files for model training
# and then output model predictions back into a TPS file

"""Loads several functions that prepare images and TPS files for model training and then write predictions to a TPS file

Troubleshooting:
    - If you are not entering absolute path names, change the working directory to a folder
    containing all the necessary python and R scripts
    - This script was written in Python 3.7 using Tensorflow 1.15.0; Tensorflow 2 will cause errors
    - Dependencies include: os, glob, PIL, tensorflow, numpy, py_tps, matplotlib, scipy, cv2, and subprocess
    - This script assumes images are named in the format GENUS_SPECIES-POP-# (e.g. "BUN_SHUB-1-01.tif")
      and TPS files are named in the format "BUN_SHUB-1-digitized.tps"
"""

#Makes a folder with image and mask subfolders for every training image
def MakeFolders (PathtoImages, WorkingDirectory, TIF=True):
    """MakeFolders makes a folder in the working directory with image and mask subfolders for every training image

    Args:
        PathtoImages should be the full path name to where the TIFF or JPEG images for model training are stored
        WorkingDirectory is where new folders will be created and where we recommend all the necessary python and R scripts are saved
        TIF should be True if the original images are in TIFF format and false if they are JPEGs
    """
    import os
    import glob
    import fnmatch
    os.chdir(PathtoImages)

    if not os.path.exists((WorkingDirectory + "/Training")):
        os.mkdir((WorkingDirectory + "/Training"))
    if (TIF==True):
        for root, dirs, files in os.walk(PathtoImages):
            for file in files:
                if (fnmatch.fnmatch(file, '*.tif')):
                    SpecimenName = file.split('.')[0]
                    SpecimenName = SpecimenName.split('/')
                    SpecimenName = SpecimenName[len(SpecimenName)-1]
                    os.mkdir(WorkingDirectory + "/Training/" + SpecimenName + "/")
                    os.mkdir(WorkingDirectory + "/Training/" + SpecimenName + '/images/')
                    os.mkdir(WorkingDirectory + "/Training/" + SpecimenName + '/masks/')
    if (TIF==False):
        for root, dirs, files in os.walk(PathtoImages):
            for file in files:
                if (fnmatch.fnmatch(file, '*.jpg')):
                    SpecimenName = file.split('.')[0]
                    SpecimenName = SpecimenName.split('/')
                    SpecimenName = SpecimenName[len(SpecimenName)-1]
                    os.mkdir(WorkingDirectory + "/Training/" + SpecimenName + "/")
                    os.mkdir(WorkingDirectory + "/Training/" + SpecimenName + '/images/')
                    os.mkdir(WorkingDirectory + "/Training/" + SpecimenName + '/masks/')

#Convert tif images to png
def ToPNG (PathtoImages, WorkingDirectory, TIF=True, Crop=False):
    """ Use after MakeFolders()
        Converts TIFF or JPEG images in PathtoImages to PNG images in the working directory

    Args:
        PathtoImages should be the full path name to where the TIFF or JPEG images for model training are stored
        WorkingDirectory is the folder where the PNG images will be saved
        TIF should be True if the original images are in TIFF format and false if they are JPEGs
        Crop: The images will be resized to be square either by padding or cropping. The default is
        padding, so Crop=False by default

    Notes:
        So long as MakeFolders() has already been used, the resized PNG images will be saved to their namesake
        folders in the working directory
    """
    from PIL import Image
    import os
    import glob
    import tensorflow as tf
    import numpy as np
    import fnmatch
    os.chdir(PathtoImages)

    if (TIF==True):
        for root, dirs, files in os.walk(PathtoImages):
            for file in files:
                if (fnmatch.fnmatch(file, '*.tif')):
                    image = Image.open((root + '/' + file))
                    image_size = image.size
                    width = image_size[0]
                    height = image_size[1]
                    if (width != height):
                        if (Crop==False):
                            bigside = width if width > height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, bigside, bigside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                            image_out.save(WorkingDirectory + "/Training/" + file.split('.')[0] + '/images/' + file.replace("tif", "png"))
                        if (Crop==True):
                            shortside = width if width < height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, shortside, shortside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                            image_out.save(WorkingDirectory + "/Training/" + file.split('.')[0] + '/images/' + file.replace("tif", "png"))
                    else:
                        image.save(WorkingDirectory + "/Training/" + file.split('.')[0] + '/images/' + file.replace("tif", "png"))
    else:
        for root, dirs, files in os.walk(PathtoImages):
            for file in files:
                if (fnmatch.fnmatch(file, '*.jpg')):
                    image = Image.open(root + '/' + file)
                    image_size = image.size
                    width = image_size[0]
                    height = image_size[1]
                    if (width != height):
                        if (Crop==False):
                            bigside = width if width > height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, bigside, bigside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                            image_out.save(WorkingDirectory + "/Training/" + file.split('.')[0] + '/images/' + file.replace("jpg", "png"))
                        if (Crop==True):
                            shortside = width if width < height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, shortside, shortside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                            image_out.save(WorkingDirectory + "/Training/" + file.split('.')[0] + '/images/' + file.replace("jpg", "png"))
                    else:
                        image.save(WorkingDirectory + "/Training/" + file.split('.')[0] + '/images/' + file.replace("jpg", "png"))

# Create masks for every training image based on a tps file
def MakeMasks (PathtoImages, WorkingDirectory, PathtoTPS, PathtoTPSoo, ExcelPath=None, TIF=True, Crop=False):
    """Use after MakeFolders() and ToPNG(), creates a training mask for every image and puts it in its namesake folder
       in the working directory

    Args:
        PathtoTPS is the path to the folder containing the relevant TPS files
        PathtoImages should be the full path name to where the TIFF or JPEG images for model training are stored
        WorkingDirectory is the folder where all the prepped masks and images are being saved
        ExcelPath is the path to an excel sheet containing which specimens to include and exclude from training, optional
        Crop: The masks will be resized to be square either by padding or cropping. The default is
        padding, so Crop=False by default
        PathtoTPSoo is the full path to the R script tps-oo.R

    Note:
        If the TPS files' IDs are in the form BUN_SHUB-1-01, this function will replace them with integers (01, 02, etc)
    """

    import py_tps
    from py_tps import TPSFile, TPSImage, TPSCurve, TPSPoints
    import numpy as np
    import scipy
    import cv2 as cv
    import PIL
    import os
    import glob
    from PIL import Image
    import subprocess
    import tensorflow as tf
    import fnmatch
    WorkingDirectoryPath = WorkingDirectory + "/Training"

    # Changing the TPS IDs to integers in order to read the file
    for root, dirs, files in os.walk(PathtoTPS):
        for file in files:
            if (fnmatch.fnmatch(file, '*.tps')):
                PopName = file.split("-",-1)[0] + "-" + file.split("-",-1)[1] + "-"
                with open((PathtoTPS + '/' + file), "r") as TPSedit_in:
                    new_file = ""
                    for line in TPSedit_in:
                        if line.startswith('ID'):
                            newline = line.replace(PopName, "")
                            new_file += newline
                        else:
                            newline = line
                            new_file += newline
                    TPSedit_in.close()
                    TPSedit_out = open(file, "w")
                    TPSedit_out.write(new_file)
                    TPSedit_out.close()

    # Create mask images from tps file and save the tps files to their namesake folder in WorkingDirectory
    for root, dirs, files in os.walk(PathtoTPS):
        for file in files:
            if (fnmatch.fnmatch(file, '*.tps')):
                tps_file_in = TPSFile.read_file(file)
                ListLength = len(tps_file_in.images)

                for i in range(0,ListLength):
                    img = tps_file_in.images[i].image.split(".", maxsplit=1)
                    img = img[0]
                    if tps_file_in.images[i].curves is None:
                        print("No curve for image {ImageName}".format(ImageName=img))
                    else:
                        try:
                            subprocess.call(['Rscript', '--vanilla', "/Users/mayasamuels-fair/Desktop/NewPythonWorkingFolder/mask_from_tps.R", PathtoTPSoo, PathtoImages, WorkingDirectory, img, str(TIF), PathtoTPS],
                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                            # Resize masks
                            path = "{Folder}{ImageName}.png".format(Folder=WorkingDirectoryPath + '/' + img + '/masks/', ImageName=img)
                            image = Image.open(path)
                            image_size = image.size
                            width = image_size[0]
                            height = image_size[1]
                            image = image.convert('RGB')
                            if Crop is False:
                                bigside = width if width > height else height
                                resized_im = tf.image.resize_with_crop_or_pad(image, bigside, bigside)
                                with tf.Session() as sess:
                                    image_out = sess.run(fetches=resized_im)
                                    assert isinstance(image_out, np.ndarray)
                                image_out = Image.fromarray(image_out)
                            else:
                                shortside = width if width < height else height
                                resized_im = tf.image.resize_with_crop_or_pad(image, shortside, shortside)
                                with tf.Session() as sess:
                                    image_out = sess.run(fetches=resized_im)
                                    assert isinstance(image_out, np.ndarray)
                                image_out = Image.fromarray(image_out)
                            image_out.save(WorkingDirectory + "/Training/" + img + '/masks/' + img + '.png')
                        except:
                            print("Skipping {ImageName}".format(ImageName=img))

    print(ExcelPath)
    #if not ExcelPath is None:
        #import xlrd
        #import shutil
        #wb = xlrd.open_workbook(ExcelPath)
        #sheet = wb.sheet_by_index(0)
        #for i in range(sheet.nrows):
            #if sheet.cell_value(i,1)=="N":
                #FolderName = sheet.cell_value(i,0)
                #shutil.move((WorkingDirectory + '/Training/' + FolderName), (WorkingDirectory + '/Excluded/' + FolderName))

# Resize the TIFF images on which you want to use your trained
def ResizeTestImages (PathtoTestImages, WorkingDirectory, ImageSize, TIF=True, Crop=False):
    """This function pads or crops images to the size on which the model was trained

    Args:
        PathtoTestImages is the full path name to the folder containing the TIFF or JPEG images on which the model will be tested
        WorkingDirectory is the folder where the resized images will be saved
        ImageSize should be one integer: the width or height to which the test images should be resized
        (they must be square, so the width and height will be equivalent)
        Crop: The test images will be resized to be square either by padding or cropping. The default is
        padding, so Crop=False by default
        TIF should be True if the original images are in TIFF format and false if they are JPEGs
    """

    from PIL import Image
    import os
    import glob
    os.chdir(PathtoTestImages)
    import tensorflow as tf
    import numpy as np
    import fnmatch

    if (TIF==True):
        for root, dirs, files in os.walk(PathtoTestImages):
            for file in files:
                if (fnmatch.fnmatch(file, '*.tif')):
                    image = Image.open((root + '/' + file))
                    image_size = image.size
                    width = image_size[0]
                    height = image_size[1]
                    if (width != height):
                        if (Crop==False):
                            bigside = width if width > height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, bigside, bigside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                        if (Crop==True):
                            shortside = width if width < height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, shortside, shortside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                        resized_im = image_out.resize((ImageSize, ImageSize))
                        if not os.path.exists((WorkingDirectory + "/Test")):
                            os.mkdir((WorkingDirectory + "/Test"))
                        resized_im.save(WorkingDirectory + "/Test/" + file.replace("tif", "png"))

    if (TIF==False):
        for root, dirs, files in os.walk(PathtoTestImages):
            for file in files:
                if (fnmatch.fnmatch(file, '*.jpg')):
                    image = Image.open((root + '/' + file))
                    image_size = image.size
                    width = image_size[0]
                    height = image_size[1]
                    if (width != height):
                        if (Crop==False):
                            bigside = width if width > height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, bigside, bigside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                        if (Crop==True):
                            shortside = width if width < height else height
                            resized_im = tf.image.resize_with_crop_or_pad(image, shortside, shortside)
                            with tf.Session() as sess:
                                image_out = sess.run(fetches=resized_im)
                                assert isinstance(image_out, np.ndarray)
                            image_out = Image.fromarray(image_out)
                        resized_im = image_out.resize((ImageSize, ImageSize))
                        if not os.path.exists((WorkingDirectory + "/Test")):
                            os.mkdir((WorkingDirectory + "/Test"))
                        resized_im.save(WorkingDirectory + "/Test/" + file.replace("jpg", "png"))

def PrepareData (ControlFile=None, PathtoImages=None, WorkingDirectory=None, PathtoTestImages=None, ImageSize=None,
                 PathtoTPS=None, PathtoTPSoo=None, ExcelPath=None, TIF=None, Crop=None):

    """This function runs MakeFolders(), TIFtoPNG(), MakeMasks(), and ResizeTestImages()

    Args:
        PathtoImages should be the full path name to where the TIFF or JPEG images for model training are stored
        WorkingDirectory is the folder containing all the necessary python and R scripts, as well as
        the folders created by MakeFolders()
        PathtoTestImages is the full path name to the folder containing the images on which the model will be tested
        ImageSize should be one integer: the width or height to which the test images should be resized
        (they must be square, so the width and height should be equivalent)
        *TPS_files can take any number of full TPS_file path names separated by commas
        Crop: The test images will be resized to be square either by padding or cropping. The default is
        padding, so Crop=False by default
        PathtoTPSoo is the path to the tps-oo.R file

    Notes:
        If there is an error, each component function can also be called individually
    """

    from tqdm import tqdm
    import numpy as np

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
        PathtoImages = Args[0][2]
        WorkingDirectory = Args[1][2]
        TIF = str_to_bool(Args[2][2])
        Crop = str_to_bool(Args[3][2])
        PathtoTPS = Args[4][2]
        ImageSize = int(Args[5][2])
        PathtoTestImages = Args[6][2]
        PathtoTPSoo = Args[7][2]
        ExcelPath = Args[8][2]

    tqdm(MakeFolders(PathtoImages, WorkingDirectory, TIF))
    print("MakeFolders() is done")
    tqdm(ToPNG(PathtoImages, WorkingDirectory, TIF, Crop))
    print("TIFtoPNG() is done")
    tqdm(MakeMasks(PathtoImages, WorkingDirectory, PathtoTPS, PathtoTPSoo, ExcelPath, TIF, Crop))
    print("MakeMasks() is done")
    tqdm(ResizeTestImages(PathtoTestImages, WorkingDirectory, ImageSize, TIF, Crop))
    print("ResizeTestImages() is done")
    print("Data is ready for model training")

def WriteMultipletoTPS (ControlFile=None, FolderofContourFiles=None, WorkingDirectory=None, PopName=None, Scale=None):
    """After the model has written contour files for each specimen in a population,
    use this to combine those contour files into one

    Args:
        FolderofContourFiles is the full path to the files output by make_predictions.py
        PopName should be in the format GENUS_SPECIES-POP (e.g. BUN_SHUB-1)
        Scale is the scale that should be written in the TPS file
    """
    import numpy as np
    import py_tps
    from py_tps import TPSFile, TPSImage, TPSCurve, TPSPoints
    import cv2
    import glob
    os.chdir(FolderofContourFiles)

    if not ControlFile is None:
        FolderofContourFiles = Args[9][2]
        PopName = Args[10][2]
        Scale = int(Args[11][2])

    for root, dirs, files in os.walk(FolderofContourFiles):
        for file in files:
            if (fnmatch.fnmatch(file, '*.txt')):
                ContourFile = ContourFile.split(".")[0]
                if (TIF==True):
                    SpecimenName = ContourFile.split("_")[2] + "_" + ContourFile.split("_")[3] + ".tif"
                else:
                    SpecimenName = ContourFile.split("_")[2] + "_" + ContourFile.split("_")[3] + ".jpg"
                SpecimenID = ContourFile.split("-")[2]

        #Write TPS file
                with open(ContourFile) as f:
                    newfile = ""
                    for line in f:
                        newline = line.replace("[","")
                        newline = newline.replace("]","")
                        newfile += newline
                    f.close()
                    fnew = open(ContourFile, "w")
                    fnew.write(newfile)
                    fnew.close()

                with open(ContourFile) as f:
                    lines = f.readlines()
                    x = []
                    y = []
                    for line in lines:
                        try:
                            x += [float(line.split()[0])]
                            y += [float(line.split()[1])]
                        except:
                            ExceptMessage = "It's just blank lines causing an error"
                coords = np.empty((len(x),2))
                for i in range(len(x)):
                    coords[i] = [x[i], y[i]]
                points = TPSPoints(coords)
                curve = TPSCurve(points)
                image = TPSImage(SpecimenName, curves=[curve], id_number=SpecimenID, scale=Scale)
                tps_file = TPSFile([image])
                if not os.path.exists((WorkingDirectory + "/TPS")):
                    os.mkdir((WorkingDirectory + "/TPS"))
                tps_file.write_to_file((WorkingDirectory + '/TPS/{Name}.TPS'.format(Name=SpecimenName)))

            #Append these text files
            os.chdir((WorkingDirectory + '/TPS'))
            read_files = glob.glob("*.TPS")
            with open("{Name}.TPS".format(Name = PopName), "wb") as outfile:
                for f in read_files:
                    with open(f, "rb") as infile:
                        outfile.write(infile.read())
                        os.remove(f)