# Machine Learning & AI Applications

This repository showcases projects that implement machine learning and artificial intelligence approaches to solve complex problems across various domains. It includes deep learning models for text-to-text and image-to-text tasks, as well as classical machine learning techniques like Random Forests and Boosting.

<i>  All projects in this repository were completed in collaboration with Corrà Sara </i>

-[Ensemble Learning](#ensemble-learning)
-[Captioning Scientific Figures](#captioning-scientific-figures)
-[Named Entity Recognition](#Named-entity-recognition)

# Ensemble Learning

>** Mosquitoes classification: Random Forest and Boosting Application**

The project was inspired by the [WINGBEATS](https://www.kaggle.com/datasets/potamitis/wingbeats) and [ABUZZ](http://web.stanford.edu/group/prakash-lab/cgi-bin/mosquitofreq/the-science/figures-2/) datasets that collect sound **recording of mosquitoes** with the aim of improving the models that identify and **classify** mosquito species. This task is essential for the purposes of disease surveillance and vector control, since mosquito-borne diseases, such as Dengue fever, Zika, Malaria and others, are extremely widespread globally. 

<p align="center">
  <img src="https://github.com/dontkillourjoy/ml_projects/blob/main/ensemble_learning/wave.png" alt="An example of the time domain representation of the mosquito sound wave" width="500" height = "400"/>
  <br/>
  <i>An example of the time domain representation of the mosquito sound wave</i>
</p>

The analysis includes a complete pipeline from raw .wav **sound data** processing to final classsification outputs that is based on the wingbeat sounds of each of the 6 considered mosquito species. The dimensionality of the feature space is reduced using **PCA**, and the initial **class imbalance** is addressed using two distinct approaches for data augmentation, **artificial sound simulation** and **SCUT** under- and oversampling. The classification task is approached by using two **ensemble learning** methods: **random forests** and **boosting**. For each method, the parameters of the model fine-tuned on four distrinct classification result metrics, and the optimal random forest and boosting model (using **XGBoost** algorithm) are selected. The optimal boostimg model's performance is evaluated on the test set and is interpreted based on the **confusion matrix**, **feature gains** and **SHAP values**.


# Captioning Scientific Figures

>**Deep Neural Network Approach for Captioning Scientific Figures **

The idea for the [project](https://colab.research.google.com/drive/1eZSwvSL8C2tXADkpvJI9HF5zMuJF9-6O#scrollTo=ek33ZXQojA-i&uniqifier=1) is based on the [HuggingFace challenge](https://huggingface.co/datasets/CrowdAILab/scicap), which aimed at developing a **neural network** approach to **scientific image captioning**. The architecture for the NN implies creating an **image-to-text** pipeline trained, validated and tested on the large-scale figure-captioning dataset **SciCap**. 

<p align="center">
  <img src="https://github.com/dontkillourjoy/ml_projects/blob/main/image_captioning/sc_figures.png" alt="Six instances of the SciCap scientific figures and captions" width="500" height = "400"/>
  <br/>
  <i>Six instances of the SciCap scientific figures and captions</i>
</p>

For this research task, an encoder-decoder **CNN + LSTM** architecture was implemented, with two different specifications for the encoder based on **GoogLeNet** and **ResNet-18** **convolutional** networks. Since the dataset provides both visual and text data, it was possible to leverage both sources of data to build a comprehensive model by including the original captions as the other source of information. The **LSTM decoder** was focused on the sentence generation task to produce the final captions, with the original captions normalized, tokenized and encoded. Additionally, to account for semantic relationships between the words in the captions, the pretrained **Glove embeddings** were used. Model training was performed using the **Adam** optimizer. The project was completed using the **PyTorch** machine learning library.

# Named Entity Recognition

The [project](https://colab.research.google.com/drive/1GCJQeBvIrcRwxzf9KATYUw72laQ4y2k1) approaches the Named Entity Recognition task in the natural language processing framework. The analysis is based on the **Georgetown University Multilayer** corpus that is oriented towards contemporary English data while meeting the requirements of the corpus and computational linguistics research. The dataset consists of around 22,500 tokens utilising the  **IOB2** format, with **11 entity types** represented. 

<p align="center">
  <img src="https://github.com/dontkillourjoy/ml_projects/blob/main/nlp_ner/sentences_eg.png" alt="Instances of sentences from GUM corpus, names entities and their distribution" width="500" height = "400"/>
  <br/>
  <i>[Forest plot for empirical logits](https://github.com/dontkillourjoy/ml_projects/blob/main/nlp_ner/sentences_eg.png)</i>
</p>

To efficiently approach the NER task, the transformer encoder-decoder architecture was used: the Bidirectional Encoder Representations from Transformers (**BERT**) pre-trained on the BooksCorpus and English Wikipedia was used to leverge the concept of transfer learning. This model was fine-tuned on the GUM dataset, with k-fold cross-validation implemented to parameter tuning. For the optimization task, the performance of three PyTorch implementations of optimizers **Adam**, AdamW and **Ranger** was compared, with the Ranger being selected as the optimal. Several **dashboards** were created for better visual representation of both the dataset and the final **evaluation metrics**. The project was completed using the **PyTorch** machine learning library.

