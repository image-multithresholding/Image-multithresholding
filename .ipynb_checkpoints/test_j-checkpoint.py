import src.image_multi_thresholding.thresholding_base as tb
import src.image_multi_thresholding.thresholding_candidates as tc

img = tb.load_image('src/tests/image_01.jpg')
mcc = tc.threshold_mec(img, 2)
print(mcc)