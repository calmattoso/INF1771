__author__ = 'Carlos & Leonardo'

import feature_extractor as extractor
import numpy as np
import os
import arff

attributes = [
  'contour_area_ratio', 'rectangle_area_ratio', 'aspect_ratio',\
  'ellipse_angle', 'fixpt_depth', 'centroid_x' , 'centroid_y' , 'hu_moment_1',\
  'hu_moment_2', 'hu_moment_3', 'hu_moment_4', 'hu_moment_5', 'hu_moment_6',\
  'hu_moment_7', 'convex_hull_perimeter', 'ellipse_center_x',\
  'ellipse_center_y', 'ellipse_major_axis', 'ellipse_minor_axis'
]  

data = list()  # Base for the Arff file
data_temp = list()
root = "../data"

sets_paths = os.listdir( root )

for set_folder in sets_paths:  #

  setV = root + "/" + set_folder

  classes = os.listdir( setV )

  class_id = 0
  for class_name in classes:
    classes_path  = setV + "/" + class_name

    examples = os.listdir(classes_path)
    
    for example in examples:
      if not( example == "count.txt" ):
        examples_path = classes_path + "/" + example
        
        images_names = os.listdir(examples_path)        

        ratio_step = int((images_names.__len__()-1)/35)

        for w in range(0, 35):
          h = w * ratio_step
          if not(images_names[h].__len__() == 14):  # in the case of non image pick
            h += 1

          images_path = examples_path + "/" + images_names[h]
          features = extractor.extract_features( images_path )

          for attribute in attributes:
            data_temp.append( features[ attribute ] )        

          print( images_path )
        
        data_temp.append( class_id )

        data.append(list(data_temp))

        del data_temp[:]  # clear
    
    if( class_name != "count.txt" ):
      class_id = class_id + 1

temp = list()
for i in range(0, 35):
    for j in range(0, attributes.__len__()):
        temp.append(attributes[j]+'{}'.format(i))
temp.append('class')

columns = temp

arff.dump('../logs/dataset.arff', data, \
          relation = "handGestures", names = columns)

