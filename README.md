
<!-- ABOUT THE PROJECT -->
## AdaEnsemble


AdaEnsemble is a framework for creating adaptive ensembles. An adaptive ensemble consists in a multi armed bandit algorithm that chooses between different machine learning models, and passes the data passed to the ensemble down to the selected machine learning model. The output of the selected model, given the data, is the output of the ensemble. This can be used for online model selection over supervised machine learning models, or as another layer to reinforcement learning algorithms. 

![](./basic-explanation.jpg)

### Built With

AdaEnsemble relies on [Breeze](https://github.com/scalanlp/breeze) for matrix operations, [Circe](https://circe.github.io/circe/) for constructing Json representations of ensembles and [JavaCPP](https://github.com/bytedeco/javacpp) for the intergration of [ONNX](https://onnx.ai/) models.





<!-- LICENSE -->
## License

Distributed under the [LGPL](./LGPL) License.




