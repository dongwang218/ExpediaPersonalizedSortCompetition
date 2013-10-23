import sys
from sklearn import ensemble
from sklearn import datasets
from sklearn.utils import shuffle
from sklearn import metrics
from sklearn.metrics import mean_squared_error
import pickle

mode = sys.argv[1]
svmlight = sys.argv[2]
model = sys.argv[3]
loss = sys.argv[4] if len(sys.argv) > 4 else 'lad'
prediction_file = open(sys.argv[5], 'wb') if len(sys.argv) > 5 else sys.stdout

X_train, y_train = datasets.load_svmlight_file(svmlight)
X_train = X_train.toarray()

def print_metric(loss, y_train, pred):
    if loss == 'deviance':
        fpr, tpr, thresholds = metrics.roc_curve(y_train, pred, pos_label=1)
        auc = metrics.auc(fpr, tpr)
        print "AUC: %f" % auc        
    elif loss == 'ls':
        error = metrics.mean_squared_error(y_train, )
        print "MSE: %f" % error
    elif loss == 'lad':
        error = metrics.mean_absolute_error(y_train, )
        print "MAE: %f" % error


if mode == 'train':
    if loss == 'deviance':
        params = {'n_estimators': 100, 'max_depth': 8, 'min_samples_split': 20,
          'learning_rate': 0.1, 'subsample':0.1, 'loss': loss, 'verbose': 1}
        clf = ensemble.GradientBoostingClassifier(**params)
    else:
        params = {'n_estimators': 1000, 'max_depth': 8, 'min_samples_split': 20,
          'learning_rate': 0.1, 'subsample':0.1, 'loss': loss, 'verbose': 1}
        clf = ensemble.GradientBoostingRegressor(**params)
    clf.fit(X_train , y_train)

    pred = clf.predict(X_train)
    print_metric(loss, y_train, pred)

    pickle.dump( [clf], open( model, 'w' ) )
elif mode == 'test':
    res = pickle.load( open( model ) )
    clf = res[0]
    if loss == 'deviance':
        pred = clf.predict_proba(X_train)[:,1]
    else:
        pred = clf.predict(X_train)
    print_metric(loss, y_train, pred)

    for p in pred:
        prediction_file.write("%.10f\n" % p)
