'''
Project 2.1 Feedforward Neural Networks
Author: Chunlei Zhou
Task: create and train feedforward neural networks on synthetic, separable data
    concern with architecture, hyperparameter and learning parameter tuning to
    maximize performance on the training set
Date: 2019/11/20
'''

'''
2. Classification of Separable, Synthetic data
'''

import torch
import torch.nn as nn
import torch.nn.functional as F
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

class Net_ellipse(nn.Module):

    def __init__(self):
        super(Net_ellipse, self).__init__()
        self.fc1 = nn.Linear(2, 20)
        self.fc2 = nn.Linear(20, 20)
        self.fc3 = nn.Linear(20, 20)
        self.fc4 = nn.Linear(20, 20)
        self.fc5 = nn.Linear(20, 20)
        self.fc6 = nn.Linear(20, 20)
        self.fc7 = nn.Linear(20, 2)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = F.relu(self.fc3(x))
        x = F.relu(self.fc4(x))
        x = F.relu(self.fc5(x))
        x = F.relu(self.fc6(x))
        x = self.fc7(x)
        return F.log_softmax(x,dim = -1)

class Net_hexa(nn.Module):

    def __init__(self):
        super(Net_hexa, self).__init__()
        self.fc1 = nn.Linear(2, 20)
        self.fc2 = nn.Linear(20, 20)
        self.fc3 = nn.Linear(20, 20)
        self.fc4 = nn.Linear(20, 20)
        self.fc5 = nn.Linear(20, 20)
        self.fc6 = nn.Linear(20, 20)
        self.fc7 = nn.Linear(20, 2)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = F.relu(self.fc3(x))
        x = F.relu(self.fc4(x))
        x = F.relu(self.fc5(x))
        x = F.relu(self.fc6(x))
        x = self.fc7(x)
        return F.log_softmax(x,dim = -1)

class Net_star(nn.Module):

    def __init__(self):
        super(Net_star, self).__init__()
        self.fc1 = nn.Linear(2, 20)
        self.fc2 = nn.Linear(20, 20)
        self.fc3 = nn.Linear(20, 20)
        self.fc4 = nn.Linear(20, 20)
        self.fc5 = nn.Linear(20, 20)
        self.fc6 = nn.Linear(20, 20)
        self.fc7 = nn.Linear(20, 2)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = F.relu(self.fc3(x))
        x = F.relu(self.fc4(x))
        x = F.relu(self.fc5(x))
        x = F.relu(self.fc6(x))
        x = self.fc7(x)
        return F.log_softmax(x,dim = -1)

def plot_decision_boundary(net, X, y, filename):
    x_min, x_max = -1, 1
    y_min, y_max = -1, 1
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

    criterion = nn.CrossEntropyLoss()

    '''2.1 Use Feedforward_Data_ellipse.csv to create and train a network that separates the data.'''
    # load data
    data_ellipse = pd.read_csv('./FeedForward_Data_ellipse.csv')
    X_ellipse = data_ellipse.values[:, 0:2]
    y_ellipse = data_ellipse.values[:, 2]
    X_ellipse = torch.tensor(X_ellipse, dtype=torch.float)
    y_ellipse = torch.tensor(y_ellipse, dtype=torch.long)

    # create and train network
    learning_rate = 0.1
    nepochs = 5000
    accuracy_hist = []
    loss_hist = []
    net_ellipse = Net_ellipse()
    optimizer = torch.optim.SGD(net_ellipse.parameters(), lr=learning_rate, momentum=0.9)
    data, target = X_ellipse, y_ellipse
    for epoch in range(nepochs):
        optimizer.zero_grad()
        ellipse_out = net_ellipse(data)
        loss = criterion(ellipse_out, target)
        loss.backward()
        optimizer.step()
        ellipse_out = net_ellipse(data)
        pred = ellipse_out.data.max(1)[1]  # get the index of the max log-probability
        correctidx = pred.eq(target.data)
        ncorrect = correctidx.sum()
        accuracy = ncorrect.item() / len(data)
        accuracy_hist.append(accuracy)
        loss_hist.append(loss.item())
        if epoch % 10 == 0:
            # monitor loss
            print('Epoch ', epoch, 'Loss ', loss.item())
            print('Training accuracy is ', accuracy)
    plot_decision_boundary(net_ellipse, X_ellipse, y_ellipse, '2.1 Feedforward_Data_ellipse.pdf')
    print('Best Accuracy', max(accuracy_hist), accuracy_hist[-1], accuracy_hist.index(max(accuracy_hist)))
    print('Best Cross-Entropy loss', min(loss_hist), loss_hist[-1], loss_hist.index(min(loss_hist)))

    '''2.2 Use Feedforward_Data_hexa.csv to create and train a network that separates the data.'''
    # load data
    data_hexa = pd.read_csv('./Feedforward_Data_hexa.csv')
    X_hexa = data_hexa.values[:, 0:2]
    y_hexa = data_hexa.values[:, 2]
    X_hexa = torch.tensor(X_hexa, dtype=torch.float)
    y_hexa = torch.tensor(y_hexa, dtype=torch.long)

    # create and train network
    learning_rate = 0.1
    nepochs = 10000
    accuracy_hist = []
    loss_hist = []
    net_hexa = Net_hexa()
    optimizer = torch.optim.SGD(net_hexa.parameters(), lr=learning_rate, momentum=0.9)
    data, target = X_hexa, y_hexa
    for epoch in range(nepochs):
        optimizer.zero_grad()
        hexa_out = net_hexa(data)
        loss = criterion(hexa_out, target)
        loss.backward()
        optimizer.step()
        hexa_out = net_hexa(data)
        pred = hexa_out.data.max(1)[1]  # get the index of the max log-probability
        correctidx = pred.eq(target.data)
        ncorrect = correctidx.sum()
        accuracy = ncorrect.item() / len(data)
        accuracy_hist.append(accuracy)
        loss_hist.append(loss.item())
        if epoch % 10 == 0:
            # monitor loss
            print('Epoch ', epoch, 'Loss ', loss.item())
            print('Training accuracy is ', accuracy)
    plot_decision_boundary(net_hexa, X_hexa, y_hexa, '2.2 Feedforward_Data_hexa.pdf')
    print('Best Accuracy', max(accuracy_hist), accuracy_hist[-1], accuracy_hist.index(max(accuracy_hist)))
    print('Best Cross-Entropy loss', min(loss_hist), loss_hist[-1], loss_hist.index(min(loss_hist)))

    '''2.3 Use Feedforward_Data_star.csv to create and train a network that separates the data.'''
    # load data
    data_star = pd.read_csv('./Feedforward_Data_star.csv')
    X_star = data_star.values[:, 0:2]
    y_star = data_star.values[:, 2]
    X_star = torch.tensor(X_star, dtype=torch.float)
    y_star = torch.tensor(y_star, dtype=torch.long)

    # create and train network
    learning_rate = 0.1
    nepochs = 10000
    accuracy_hist = []
    loss_hist = []
    net_star = Net_star()
    optimizer = torch.optim.SGD(net_star.parameters(), lr=learning_rate, momentum=0.9)
    data, target = X_star, y_star
    for epoch in range(nepochs):
        optimizer.zero_grad()
        star_out = net_star(data)
        loss = criterion(star_out, target)
        loss.backward()
        optimizer.step()
        star_out = net_star(data)
        pred = star_out.data.max(1)[1]  # get the index of the max log-probability
        correctidx = pred.eq(target.data)
        ncorrect = correctidx.sum()
        accuracy = ncorrect.item() / len(data)
        accuracy_hist.append(accuracy)
        loss_hist.append(loss.item())
        if epoch % 10 == 0:
            # monitor loss
            print('Epoch ', epoch, 'Loss ', loss.item())
            print('Training accuracy is ', accuracy)
    plot_decision_boundary(net_star, X_star, y_star, '2.3 Feedforward_Data_star.pdf')
    print('Best Accuracy', max(accuracy_hist), accuracy_hist[-1], accuracy_hist.index(max(accuracy_hist)))
    print('Best Cross-Entropy loss', min(loss_hist), loss_hist[-1], loss_hist.index(min(loss_hist)))