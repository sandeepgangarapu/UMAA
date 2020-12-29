import pandas as pd
import pickle


valid_data = pd.read_csv('all_data_for_final.csv')
valid_data = valid_data.set_index('ID_DEMO')


# load the model from disk
loaded_model = pickle.load(open('finalized_model.sav', 'rb'))

a = loaded_model.predict_proba(valid_data)[:,1]
b = pd.DataFrame({'ID_DEMO':valid_data.index, 'prob': a})
b.to_csv('valid_predict_proba.csv')