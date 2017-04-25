import cgi

input = cgi.FieldStorage() # Get input

# Get the fields we want
filename = input["filename"]
lines = int(input["lines"]) # Cast to int

# HTML output
print("Content-type: text/html\n\n")
print("<html>")
print("<header><title>Products</title></header>")
print("<body>")
# Try opening CSV file
try:
    file = fopen(filename,"r")
except: # Error
    print("File not found")
else:
    print("<table>")
    print("<tr>")
    print("<th> Product Code </th>")
    print("<th> Quantity in Stock</th")
    print("<th> Min in Stock</th>")
    print("<th>Unit Price</th>")
    print("<th> Min to Purchase</th>")
    for i in range(lines): # Loop through lines
        data = file.readline()
        # Tokenize
        data = data.split(",")
        code = data[0]
        quantity = data[1]
        min = data[2]
        unit = data[3]
        min2 = min-quantity
        if quantity<min:
            print("<tr>")
            print("<td> %d </td>"%(data))
            print("<td> %d </td>"%(quantity))
            print("<td> %d </td>"%(min))
            print("<td> %d </td>"%(unit))
            print("<td> %d </td>"%(min2))
            print("</tr>")

    print("</table>")
print("</body>")
print("</html>")
