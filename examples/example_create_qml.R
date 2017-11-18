## -- create qml-file based on 'classaccur'-object:

# 1) get optimal classification scheme:
hsm<- HSMclass(refdata, predictions, nclasses = 6,
               iterations = 1000, coolfactor=99, InitTemp = 80,
               weight.norefs = 2, weight.classwidth = 2)

# 2) evaluate classification scheme:
acc.opti<- classAccuracy(refdata, predictions, def.int = hsm$best.classbreaks)

# # 3) write qml-file for classification scheme:
# create_qml(acc.opti, out.file = "opti_class.qml")
