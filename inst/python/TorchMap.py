import torch

def map_python_initiate(maxCol,maxRow, maxT=None):
  if maxT != None:
    matrix = torch.sparse.FloatTensor(torch.LongTensor([[0,0],[0,1],[0,0]]), torch.FloatTensor([0.,0.]), torch.Size([maxRow,maxCol,maxT]))
  else:
    matrix = torch.sparse.FloatTensor(torch.LongTensor([[0,0],[0,1]]), torch.FloatTensor([0.,0.]), torch.Size([maxRow,maxCol]))
  return matrix
  
def map_python(matrix, datas, maxCol,maxRow, maxT=None):
  if maxT != None:
    indexes= datas[:,0:3]-1
    matrixt = torch.sparse.FloatTensor(torch.LongTensor(indexes.T), torch.FloatTensor(datas[:,3]), torch.Size([maxRow,maxCol, maxT]))
    matrix = matrix.add(matrixt)
  else:
    indexes= datas[:,0:2]-1
    matrixt = torch.sparse.FloatTensor(torch.LongTensor(indexes.T), torch.FloatTensor(datas[:,2]), torch.Size([maxRow,maxCol]))
    matrix = matrix.add(matrixt)
  return matrix  
