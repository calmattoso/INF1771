__author__ = 'Carlos & Leonardo'

import numpy as np
import scipy
import cv2

def show_image( image , delay = 5000, text = 'popup' ):
  cv2.imshow(text,image) 
  k = cv2.waitKey( delay )
  return

def remove_noises(image, dilations_itr = 3, erosions_itr = 2):
  kernel = np.ones((3,3), np.uint8)

  image = cv2.morphologyEx(image, cv2.MORPH_DILATE, kernel, \
               iterations = dilations_itr);
   
  image = cv2.morphologyEx(image, cv2.MORPH_ERODE, kernel, \
                iterations = erosions_itr);

  return image

def extract_features( path ):
  d = 10000

  # Open image
  image = cv2.imread( path )
  #show_image(image)

  # Reduce lightness to 75% its original value
  hls = cv2.cvtColor(image,cv2.COLOR_BGR2HLS)
  hls[:,:,1] = hls[:,:,1] * 0.75
  #show_image(hls)

  image = cv2.cvtColor(hls,cv2.COLOR_HLS2BGR)

  # Convert to grayscale
  gray = cv2.cvtColor(image,cv2.COLOR_BGR2GRAY)
  #show_image( gray )

  # Perform a very nice median blur
  blur = cv2.medianBlur(gray,11)
  #show_image(blur)

  # Binarization
  _,thresh = cv2.threshold(blur, 90,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
  #show_image(thresh)

  # Remove noises and overlap through morphology meshing
  morphed = remove_noises(thresh)
  #show_image(morphed)

  contours , _ = cv2.findContours(morphed, cv2.RETR_LIST, \
                                  cv2.CHAIN_APPROX_NONE)
  
  preserve = morphed.copy()
  #cv2.drawContours( morphed , contours , -1, (255, 127, 0))
  #show_image(  morphed , d)

  # Get largest contour
  max_area = -1
  largest_contour = 0

  for i in range(0, contours.__len__()):
    area = cv2.contourArea( contours[i] ) 
    if area > max_area:
        max_area = area
        largest_contour = contours[i]

  #cv2.drawContours( preserve , [largest_contour] , -1, (255, 127, 0))
  #show_image( preserve , d )

  # Get and show convex hull
  convex_hull = cv2.convexHull( largest_contour , clockwise = False )  
  contour_area_ratio = \
    ( cv2.contourArea(largest_contour) / cv2.contourArea(convex_hull))

  # Get aspect ratio of smallest surrounding rectangle
  rectangle = cv2.minAreaRect( largest_contour )
  aspect_ratio = rectangle[1][0]/float( rectangle[1][1] )
  rectangle_area_ratio = \
    (cv2.contourArea(convex_hull) / (rectangle[1][0] * float(rectangle[1][1])))

  # Get Hu moments
  moments = cv2.moments(convex_hull, True)
  hu_moments = cv2.HuMoments( moments )
  
  #print( cv2.contourArea( contour ) )    
  #print( cv2.contourArea(convex_hull ) )

  # Get distance of furthest defect
  defects = cv2.convexityDefects( largest_contour, \
                  cv2.convexHull( largest_contour, returnPoints=False) )
  
  max_fixpt_depth = -10000000
  for defect in defects:
    if defect[0][3] > max_fixpt_depth:
      max_fixpt_depth = defect[0][3]

  # Get and show convex hull
  convex_hull = cv2.convexHull( largest_contour , clockwise = False )  
  contour_area_ratio = \
    ( cv2.contourArea(largest_contour) / cv2.contourArea(convex_hull))

  # Get aspect ratio of smallest surrounding rectangle
  rectangle = cv2.minAreaRect( largest_contour )
  aspect_ratio = rectangle[1][0]/float( rectangle[1][1] )
  rectangle_area_ratio = \
    (cv2.contourArea(convex_hull) / (rectangle[1][0] * float(rectangle[1][1])))

  # Get Hu moments
  moments = cv2.moments(convex_hull, True)
  hu_moments = cv2.HuMoments( moments )
  
  #print( cv2.contourArea( contour ) )    
  #print( cv2.contourArea(convex_hull ) )

  # Get distance of furthest defect
  defects = cv2.convexityDefects( largest_contour, \
                  cv2.convexHull( largest_contour, returnPoints=False) )
  
  max_fixpt_depth = -10000000
  for defect in defects:
    if defect[0][3] > max_fixpt_depth:
      max_fixpt_dept = defect[0][3]

  # Get fitted ellipse angle and centers
  ellipse = cv2.fitEllipse( largest_contour )
  ellipse_angle = ellipse[ 2 ]
  ellipse_center_x   = ellipse[0][0]
  ellipse_center_y   = ellipse[0][1]
  ellipse_major_axis = ellipse[1][0]
  ellipse_minor_axis = ellipse[1][1]

  # Get hull centroid
  cx = int(moments['m10']/moments['m00'])
  cy = int(moments['m01']/moments['m00'])


  # Finally, get convex hull perimeter
  perimeter = cv2.arcLength(convex_hull, True)

  # Prepare features json 
  base_features = {
    'contour_area_ratio': contour_area_ratio,
    'rectangle_area_ratio': rectangle_area_ratio,
    'aspect_ratio': aspect_ratio,
    'convex_hull_perimeter': perimeter,
    'ellipse_angle': ellipse_angle,
    'ellipse_center_x': ellipse_center_x ,
    'ellipse_center_y': ellipse_center_y,
    'ellipse_major_axis': ellipse_major_axis,
    'ellipse_minor_axis': ellipse_minor_axis,
    'fixpt_depth': max_fixpt_depth/256.0,
    'centroid_x': cx,
    'centroid_y': cy,
    'hu_moment_1': hu_moments[0][0],
    'hu_moment_2': hu_moments[1][0],
    'hu_moment_3': hu_moments[2][0],
    'hu_moment_4': hu_moments[3][0],
    'hu_moment_5': hu_moments[4][0],
    'hu_moment_6': hu_moments[5][0],
    'hu_moment_7': hu_moments[6][0]
  }

  return base_features
