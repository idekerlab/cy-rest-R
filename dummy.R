data1 <- '{"data" : {"name" : "dddd11111"},"elements" : {"nodes" : [],"edges" : []}}'
#res <- POST(url="http://localhost:8080/v1/networks", body = data1, encode = "json")

net_data = list(name="foo3", id="foo3")
dt = list(name="aaa", id="aaa")
n1 = list(data=dt)
nd = list(n1)
ed = list()
elm = list(nodes=nd, edges=ed)


dummy = toJSON(list(data=net_data, elements=elm))