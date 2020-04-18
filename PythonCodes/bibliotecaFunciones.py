from skimage import io,util,exposure
import numpy as np
import numpy.ma as ma

def thresholdedImage(img, thr):
    
    """
thresholdedImage function

Arguments:
img an cimg object
thr a list of thresholds

Value:
thresholdingImage returns an object with class "cimg" and possible values 0,1,...,255

"""

    #Convert image to data
    
    try:
        image = io.imread(img, as_gray=True)
    except:
        print("[Error!]: incorrect image path and/or name\n") #Error check
        print("[Error!]: other possible error involves incorrect 'img' (image) argument data type") #Error check
        exit()

    #Convert threshold to numpy array

    thr = np.array(thr, dtype=int)

    _, counts = np.unique(thr, return_counts=True) 
    for count in counts:
        if(count == 1):
            continue
        print("[Error!]: there are repetead values in 'thr' (list of thresholds) argument") #Error check
        exit()

    # Normalize gray levels in the range 0,1,...,255
    
    image = util.img_as_ubyte(image)
    
    # Find and sort gray levels
    
    _, grays = exposure.histogram(image)
    
    # Find amount of gray levels and thresholds

    L = grays.size
    
    K = thr.size
    
    if(K == 0):
        print("[Error!]: 'thr', list of thresholds, is empty") #Error check
        exit()
    if(K>L):
        print("[Error!]: 'thr', list of thresholds, contains more values than the amount of gray levels") #Error check
        exit()

    # Compute the grayscale mean in each class

    avg = np.zeros(1)
    avg[0] = np.mean(grays[0:(thr[0]+1)])
    if(K != 1):
        for i in range(1, K):
            avg = np.append(avg, np.mean(grays[(thr[i-1]+1):(thr[i]+1)]))
    avg = np.append(avg, np.mean(grays[(thr[K-1]+1):]))

    # Round the means

    avg = np.round(avg)

    # Replace each gray value in the image by the mean of its class

    np.place(image, ma.masked_inside(image, grays[0], grays[thr[1]]), avg[0])
    if(K != 1):
        for i in range(1, K):
            np.place(image, ma.masked_inside(image, grays[thr[i-1]+1], grays[thr[i]]), avg[i])
    np.place(image, ma.masked_inside(image, grays[thr[K-1]+1], grays[L-1]), avg[K])


    #Output

    return image
