'''
Project 2.1 Feedforward Neural Networks
Author: Chunlei Zhou
Task: create and train feedforward neural networks on synthetic, separable data
    concern with architecture, hyperparameter and learning parameter tuning to
    maximize performance on the training set
Date: 2019/11/19
'''

'''
1. Classification of XOR Data
'''

import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import matplotlib.pyplot as plt

class XOR(nn.Module):

    def __init__(self):
        super(XOR, self).__init__()
        self.fc1 = nn.Linear(2, 2)
        #self.fc2 = nn.Linear(2, 2)
        #self.fc3 = nn.Linear(2, 2)
        self.fc4 = nn.Linear(2, 2)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        #x = F.relu(self.fc2(x))
        #x = F.relu(self.fc3(x))
        x = self.fc4(x)
        return F.log_softmax(x,dim=-1)

def plot_decision_boundary(net, X, y, filename):
    x_min, x_max = -0.5, 1.5
    y_min, y_max = -0.5, 1.5
    h = 0.01
    # Generate a grid of points with distance h between them
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    # Predict the function value for the whole gid
    X_out = net(torch.tensor(np.c_[xx.ravel(), yy.ravel()], dtype=torch.float))
    Z = X_out.data.max(1)[1]
    # Z.shape
    Z = Z.reshape(xx.shape)
    # Plot the contour and training examples
    plt.contourf(xx, yy, Z, cmap=plt.cm.Spectral)
    plt.scatter(X[:, 0], X[:, 1], c=y, cmap=plt.cm.Spectral, s=1)
    plt.savefig(filename)
    plt.close()

if __name__ == "__main__":
    # Create arrays containing the input data and the corresponding output labels for the XOR operator
    X = np.array([[0, 0], [0, 1], [1, 0], [1, 1]])
    y = [0,1,1,0]
    X = torch.tensor(X, dtype=torch.float)
    y = torch.tensor(y, dtype=torch.long)
    # Create and train a network with at least three hidden layers that separates the XOR data
    xor = XOR()
    # create a stochastic gradient descent optimizer
    learning_rate = 0.1
    optimizer = torch.optim.SGD(xor.parameters(), lr=learning_rate, momentum=0.9)
    # create a loss function
    criterion = nn.CrossEntropyLoss()
    # train
    nepochs = 10000
    data, target = X, y
    # run the main training loop
    for epoch in range(nepochs):
        optimizer.zero_grad()
        # forward propagate
        xor_out = xor(data)
        # compute loss
        loss = criterion(xor_out, target)
        # backpropagate
        loss.backward()
        # update parameters
        optimizer.step()
        xor_out = xor(data)
        pred = xor_out.data.max(1)[1]  # get the index of the max log-probability
        correctidx = pred.eq(target.data)
        ncorrect = correctidx.sum()
        accuracy = ncorrect.item() / len(data)
        '''if accuracy == 1.0:
            print('Epoch ', epoch, 'Loss ', loss.item())
            print('Training accuracy is ', accuracy)
            break'''
        if epoch % 10 == 0:
            # monitor loss
            print('Epoch ', epoch, 'Loss ', loss.item())
            print('Training accuracy is ', accuracy)
        # a. plotting the network outputs in a densely sampled region around [-0.5,1.5] * [-0.5,1.5]
        '''use the following if block for problem a'''
        '''if accuracy == 1.0:
            plot_decision_boundary(xor, X, y, 'XOR_Problem_2a.pdf')
            break'''
        # b. Plot the decision boundaries of a network after the cross-entropy loss falls below 1*10^(-4)
        '''use the following if block for problem b'''
        if loss.item() < 0.0001:
            plot_decision_boundary(xor, X, y, 'XOR_Problem_2b.pdf')
            break