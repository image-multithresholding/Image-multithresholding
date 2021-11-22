import unittest
import src.image_multi_thresholding.base as base
import numpy as np
from skimage import io


class TestThresholdedImage(unittest.TestCase):

    def setUp(self):
        self.image01 = base.load_image('PythonCodes/tests/image_01.jpg')
        # Check test_image_01.jpg is 600x600px
        self.assertEquals(self.image01.shape, (600, 600))

    def test_arguments(self):
        with self.assertRaises(Exception):
            # Should have at least one threshold
            base._thresholded_image(self.image01, [])
        with self.assertRaises(Exception):
            # Should receive a list of thresholds
            base._thresholded_image(self.image01, 123)

    def test_number_of_classes(self):
        timage = base._thresholded_image(self.image01, [100])
        # Check thresholded image has no more than 2 classes
        self.assertLessEqual(self.count_classes(timage), 2)
        timage = base._thresholded_image(self.image01, [100, 200])
        # Check thresholded image has no more than 3 classes
        self.assertLessEqual(self.count_classes(timage), 3)

    def test_class_values(self):
        # Has two classes, with means 64 (127 - 0 // 2) and 192 (255 - 127 // 2)
        timage = base._thresholded_image(self.image01, [127])
        classes = self.get_classes(timage)
        for value in [64, 192]:
            self.assertIn(value, classes)

    def count_classes(self, timage):
        gray_values = set()
        for value in timage.flat:
            gray_values.add(value)
        return len(gray_values)

    def get_classes(self, timage):
        gray_values = set()
        for value in timage.flat:
            gray_values.add(value)
        return gray_values


class TestPSNR(unittest.TestCase):

    def setUp(self):
        self.image01 = base.load_image('PythonCodes/tests/image_01.jpg')
        self.thresholded01 = base._thresholded_image(self.image01, [127])
        self.PSNR = base.PSNR(self.image01, self.thresholded01)

    def test_return(self):
        self.assertEquals(type(self.PSNR), np.float64)
        self.assertGreaterEqual(self.PSNR, 0)


if __name__ == '__main__':
    unittest.main()
