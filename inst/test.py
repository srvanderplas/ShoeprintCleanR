import numpy as np
import cv2
from matplotlib import pyplot as plt

img = cv2.imread('001351R_20171211_5_1_1_csafe_jekruse.tif',0)

edges = cv2.Canny(img,50,100)

plt.subplot(121),plt.imshow(img,cmap = 'gray')
plt.title('Original Image'), plt.xticks([]), plt.yticks([])
plt.subplot(122),plt.imshow(edges,cmap = 'gray')
plt.title('Edge Image'), plt.xticks([]), plt.yticks([])

plt.show()

ret,thresh = cv2.threshold(img,190,255,0)
image, contours, hierarchy = cv2.findContours(thresh,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)

plt.subplot(121),plt.imshow(img,cmap = 'gray')
plt.title('Original Image'), plt.xticks([]), plt.yticks([])
plt.subplot(122),plt.imshow(image,cmap='gray')
plt.title('Original Image'), plt.xticks([]), plt.yticks([])
plt.show()

imgc = cv2.drawContours(img, contours, -1, (0,255,0), 3)
plt.subplot(121),plt.imshow(img,cmap = 'gray')
plt.title('Original Image'), plt.xticks([]), plt.yticks([])
plt.subplot(122),plt.imshow(imgc,cmap='gray')
plt.title('Original Image'), plt.xticks([]), plt.yticks([])
plt.show()