import PythonCodes.library.thresholding_base as tb
import PythonCodes.library.thresholding_candidates as tc

img = tb.load_image('PythonCodes/tests/image_01.jpg')
mcc = tc.threshold_mec(img, 2)
print(mcc)