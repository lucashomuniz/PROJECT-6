# ✅ PROJECT-6

This project aims to develop a predictive model to assess the credit risk of customers in a banking institution. The purpose is to demonstrate the construction process of a classification model capable of predicting the risk associated with granting credit to bank customers. We will use a data set to train and build the model, aiming to predict the risk presented by each client when applying for a credit line. The accuracy of the predictive model is crucial, since granting credit to a customer with low payment capacity can result in significant losses for the bank. The target variable of this project is CreditStatus. In addition, we intend to compare machine learning algorithms available in Azure, such as Bayesian, Neural Network and SVM.

This project was developed in parallel using both the R programming language and Microsoft Azure Machine Learning Studio. R language provides a powerful and flexible environment for statistical analysis and modeling, while Azure Machine Learning Studio offers a cloud-based platform for building and deploying machine learning models. By leveraging these tools, the team could benefit from the strengths of each platform and choose the most suitable one based on their requirements and preferences. This approach demonstrates the versatility and adaptability of the project, as it was able to leverage different tools to achieve the desired outcome.

Keywords: R Language, Microsoft Azure Big Data, Data Analysis, S.M.O.T, Bayesian, Neural Network, S.V.M, Credit Analysis, Bank

# ✅ PROCESS

Microsoft Azure Machine Learning offers a cloud-based platform with scalable computing resources, seamless integration with other Azure services, and automated machine learning capabilities for model selection and tuning. It provides tools for managing experiments, tracking performance, and version control, facilitating reproducibility and collaboration. With simplified model deployment, monitoring, and extensive library support, it enables efficient development and integration of machine learning workflows. Azure Machine Learning ensures security and compliance, while promoting collaboration and knowledge sharing among team members. Overall, it is a powerful platform for developing, deploying, and managing machine learning models in the cloud.

![image](https://github.com/lucashomuniz/PROJECT-6/assets/123151332/8b6d115a-e01a-4347-9cef-0bc30b326580)

Initially, the business problem is defined and the dataset is selected, such as the German Credit UCI dataset. Next, data pre-processing is carried out, converting categorical variables, applying attribute engineering and quantization techniques. Exploratory analysis is conducted to understand the characteristics of the data. Balancing is performed using SMOTE. As mentioned earlier, an important step is Dataset Balancing with the Synthetic Minority Oversampling Technique. The variable “CreditStatus”, which is a categorical variable (and also the target of this predictive model), has a frequency distribution where about 70% have Status 1 “Good” and 30% Status 2 “Bad”. Allowing the frequency distribution of this variable to remain this way can lead to problems. If it is presented in this way to the predictive model, it may become biased, because it will learn much more about what a “Good Payer” is than about a “Bad Payer”. With this, we will apply the technique of balancing classes, with SMOTE. As a result, the distribution becomes more balanced and now there is a high chance that the predictive model generated in the future will not be so biased.

![image](https://github.com/lucashomuniz/PROJECT-6/assets/123151332/d5b8f5f9-b2f1-46e1-9ac1-4367dfe73f0e)

Next, variable selection and filter methods are introduced. As explained earlier, this project is developed in parallel both in R Language and in Azure Machine Learning. Thus, within Azure Machine Learning, two packages were created to select the variables, namely: “Filter Based Feature Selection” and “Permutation Feature Importance”. A third algorithm for the selection of the main variables was developed in Language R, when comparing the three, it is clear that the algorithm written in Language R with Random Forrest has a better performance, and therefore we will continue with it.

![image](https://github.com/lucashomuniz/PROJECT-6/assets/123151332/0dcafa63-e4c9-4a5d-85d1-0885083ab6e9)

The experiment is built around four algorithms (three in Azure and one in R). The three predictive models of Azure Machine Learning are Bayesian, Neural Network and SVM. Models are trained, their results interpreted and evaluated with appropriate metrics. Comparing the models helps to identify the most effective one. 

![image](https://github.com/lucashomuniz/PROJECT-6/assets/123151332/072ded70-1d67-4ab7-a9b4-adf83ad17522)

Initially comparing the Bayesian Algorithm (Blue Line) and the Neural Network Algorithm (Red Line). Looking at the graph of the R.O.C Curve, it is possible to notice that the Bayesian Algorithm has a better performance, with an accuracy of almost 68%. Comparing the Neural Network Algorithm (Blue Line) with the SVM Algorithm (Red Line). Looking at the graph of the R.O.C Curve, it is possible to notice that the SVM Algorithm has a better performance, with an accuracy of 64%. Among the three models, the one with the best result was the Bayesian Algorithm and the one with the worst result was the Neural Network Algorithm. Finally, a script was created in R Language to develop a machine learning algorithm and thus compare the results with those obtained within Azure Machine Learning. After developing the model training, the model test was elaborated. With this, it was possible to reach an accuracy of approximately 77%, surpassing the accuracy of the algorithms present in Azure Machine Learning.

![image](https://github.com/lucashomuniz/PROJECT-6/assets/123151332/7c2334e9-b02c-4cb5-93ea-4387c1e9039c)

# ✅ CONCLUSION

Experimenting with multiple predictive models simultaneously is one of the main advantages of using Azure Machine Learning. To make it possible to do the same within the R language, it would be much more laborious. The ideal is to initially use Azure, in an experimentation phase, in order to understand the best algorithm, it is indicated to use Azure. Later, once you have already decided which machine learning algorithm to use, it is recommended to migrate to the R language so that customization and optimization can be better performed.

# ✅ DATA SOURCERS

The “German Credit Data” dataset will be used to build and train the model in this experiment. This dataset is based on real data generated by a researcher at the University of Hamburg, Germany. The dataset contains 1000 observations and 20 variables, representing customer data, such as: current account status, credit history, current credit amount, employability, residence, age, etc. 

Dataset: https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)
