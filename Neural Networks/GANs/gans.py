# -*- coding: utf-8 -*-
"""
Created on Fri Jun  3 11:56:38 2022
Title: Experimenting with Geneartive Adversarial Networks
@author: sergi
Description: We estimate a GAN using TensorFlow.
"""

from numpy.random import randn
import os
import tensorflow       as tf
from   tensorflow       import keras
from   tensorflow.keras import layers

randn(256*256).reshape(,)

# Reading the data
wd         = "E:\\Github\\arquivoModelagem\\Neural Networks\\GANs\\gan-getting-started"
os.chdir(wd)
my_dir     = "E:\\Github\\arquivoModelagem\\Neural Networks\\GANs\\gan-getting-started\\monet_jpg" 
img_height = 256
img_width  = 256

train_ds = keras.utils.image_dataset_from_directory(
  os.getcwd(),
  validation_split=0.2,
  subset="training",
  seed=123,
  image_size=(img_height, img_width),
  batch_size=32, 
  labels='inferred')

# Generator
model = keras.Sequential(
    [
        layers.Dense(2, activation="relu", name="layer1"),
        layers.Dense(3, activation="relu", name="layer2"),
        layers.Dense(4, name="layer3"),
    ]
)

# Discriminator
model = keras.Sequential(
    [
        layers.Dense(2, activation="relu", name="layer1"),
        layers.Dense(3, activation="relu", name="layer2"),
        layers.Dense(4, name="layer3"),
    ]
)