import numpy as np
from PIL import Image

def image_to_matrix(image_path, target_size):
    try:
        # Open the image
        with Image.open(image_path) as img:
            # Convert the image to grayscale
            img_gray = img.convert('L')
            
            # Resize the image to the target size
            img_resized = img_gray.resize(target_size, Image.LANCZOS)
            
            # Convert the image to a numpy array
            img_matrix = np.array(img_resized)
            
        return img_matrix
    except IOError:
        print(f"Error: Unable to open the image file {image_path}")
        return None

# Define the paths to your images
image1_path = 'x1.bmp'
image2_path = 'y4.bmp'

# Define the target size (we'll use the larger width and height)
target_size = (256, 188)

# Convert both images to matrices
matrix1 = image_to_matrix(image1_path, target_size)
matrix2 = image_to_matrix(image2_path, target_size)

if matrix1 is not None and matrix2 is not None:
    print(f"Shape of matrix1: {matrix1.shape}")
    print(f"Shape of matrix2: {matrix2.shape}")

    # You can now use these matrices for further processing
    # For example, to get the average brightness of each image:
    print(f"Average brightness of image1: {np.mean(matrix1)}")
    print(f"Average brightness of image2: {np.mean(matrix2)}")
else:
    print("Error occurred while processing the images.")