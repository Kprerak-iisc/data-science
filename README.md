# data-science
Data science Projects 

Amazon.ipynb :  Dataset : https://www.kaggle.com/c/planet-understanding-the-amazon-from-space/data
• To label satellite images provided by Planet and SCCON with atmospheric conditions and various classes of land cover/use. 
• Developed a convolutional neural network for multi-label photo classification to achieve F1 score of 0.837 using only 10% of data.
• Used Transfer learning to improve the model performance using VGG-16 model to achieve F1 score of 0.886 with only 10% of data.

BiDAF_QA_SQuAD.ipynb : Dataset: https://github.com/rajpurkar/SQuAD-explorer/tree/master/dataset
Implemented Bi-directional attention flow(BiDAF) network from scratch as proposed in Minjoon Seo et. Al on Stanford Question Answering Dataset to build a closed-domain, extractive Q&A model which can extract a span of text from the context as the answer.

Mnsit_CNN.ipynb : Dataset: http://yann.lecun.com/exdb/mnist/
The MNIST problem is a dataset developed by Yann LeCun, Corinna Cortes and Christopher Burges for evaluating machine learning models on the handwritten digit classification problem. The dataset was constructed from a number of scanned document dataset available from the National Institute of Standards and Technology (NIST). This is where the name for the dataset comes from, as the Modified NIST or MNIST dataset. Each image is a 28 by 28 pixel square (784 pixels total). A standard split of the dataset is used to evaluate and compare models, where 60,000 images are used to train a model and a separate set of 10,000 images are used to test it.
It is a digit recognition task. As such there are 10 digits (0 to 9) or 10 classes to predict. Results are reported using prediction error. Convolutional Neural Networks has been used in the model to obtain prediction error of less than 1%.

Monet2photo_cyclegan.ipynb : Dataset: https://people.eecs.berkeley.edu/~taesung_park/CycleGAN/datasets/monet2photo.zip
Studied the construct and the underlying architecture of simple GAN and CycleGAN. Implemented CycleGAN model for Painting style neural transfer using ‘monet2photo’ dataset which generated images of photos from images of Monet paintings, and vice-versa in absence of one-to-one correspondence between input and output images. 

cifar_10_imageclassification.ipynb: Dataset: https://www.cs.toronto.edu/~kriz/cifar.html
CIFAR-10 (Canadian Institute For Advanced Research) is the “hello world” type dataset of computer vision. Used deep leaning Convolutional neural network to build the model using Keras API for Image Classification with 88.6% accuracy on test set.

cred_bal.ipynb: Dataset: https://rdrr.io/cran/ISLR/man/Credit.html
•	Objective: Analyze the dataset consisting of information from credit card holders to comprehend which factors influence the Credit Card Balance of a cardholder, and to predict the average Balance of a given individual. 
•	Carried out EDA of the dataset, followed by feature selection and regression analysis to build a model which explains 96% variance. 

cyclegan_1.ipynb:  Dataset: https://people.eecs.berkeley.edu/~taesung_park/CycleGAN/datasets/horse2zebra.zip
Applied CycleGAN model on the Horse Zebra   dataset used by Zhu et al. (Research Paper: https://arxiv.org/abs/1703.10593) in keras
 





