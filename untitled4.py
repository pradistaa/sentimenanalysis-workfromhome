# -*- coding: utf-8 -*-
"""
Created on Sat Mar 20 13:48:22 2021

@author: Admin
"""

import cv2
import time

cap= cv2.VideoCapture('D:\Skripsi\Data\AM\VID_20210314_170047.mp4')


fps= int(cap.get(cv2.CAP_PROP_FPS))

if cap.isOpened() == False:
    print("Error File Not Found")

while cap.isOpened():
    ret,frame= cap.read()

    if ret == True:

        time.sleep(0.00001/fps)

        cv2.imshow('frame', frame)

        if cv2.waitKey(1) & 0xFF == ord('q'):
            break

    else:
        break


cap.release()
cv2.destroyAllWindows()