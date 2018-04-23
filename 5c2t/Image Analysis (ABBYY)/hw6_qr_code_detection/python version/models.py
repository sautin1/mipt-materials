from keras.models import Model
from keras.layers import Input, Conv2D, LeakyReLU, Dropout, MaxPool2D
from keras.regularizers import l2


class DarknetBlock:
    def __init__(self, filters1, filters3, strides=(1, 1)):
        self._filters1 = filters1
        self._filters3 = filters3
        self._strides = strides

    def __call__(self, input_tensor, *args, **kwargs):
        x = input_tensor
        for filters, size, strides in [(self._filters3, (3, 3), self._strides),
                                       (self._filters1, (1, 1), (1, 1))]:
            x = Conv2D(filters, size, strides=strides, padding='same',
                       kernel_initializer='he_normal', kernel_regularizer=l2(1e-10))(x)
            x = LeakyReLU(alpha=0.05)(x)
        return x


class DarknetModel:
    def __init__(self, input_shape, use_dropout=False, filter_counts=None):
        self._input_shape = input_shape
        self._use_dropout = use_dropout
        self._filter_counts = filter_counts or [60, 80, 100]
        self._downsample_factor = None
        self._build()

    def _build(self):
        conv_model_input = Input(shape=self._input_shape)
        x = Conv2D(10, (3, 3), strides=(2, 2), padding='same',
                   kernel_initializer='he_normal', kernel_regularizer=l2(1e-10))(conv_model_input)
        self._downsample_factor = 2
        if self._use_dropout:
            x = Dropout(0.1)(x)

        filter_counts = self._filter_counts
        for idx, filter_count in enumerate(filter_counts):
            x = DarknetBlock(filter_count, filter_count * 2)(x)
            x = DarknetBlock(filter_count, filter_count * 2)(x)
            if idx < len(filter_counts) - 1:
                x = MaxPool2D(pool_size=(2, 2), padding='valid')(x)
                self._downsample_factor *= 2
                if self._use_dropout:
                    x = Dropout(0.1)(x)
        self._model = Model(inputs=[conv_model_input], outputs=[x])

    def get_downsample_factor(self):
        return self._downsample_factor

    def get_model(self):
        return self._model

    def compile(self, loss=None, optmizer=None):
        pass
