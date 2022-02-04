# this python file is used to convert pdf to txt document
import os

from flask import Flask
import PyPDF2

app = Flask(__name__)


# Function to read and extract contents from PDF documents and
# return a text file
def convert_PDF_to_Txt(filename):
    file_name, file_extension = os.path.splitext(filename)
    print("filename is : " + file_name)
    pdffileobj = open(filename, 'rb')
    # create reader variable that will read the pdffileobj
    pdfreader = PyPDF2.PdfFileReader(pdffileobj)

    # This will store the number of pages of this pdf file
    x = pdfreader.numPages
    text = ""
    # create a variable that will select the selected number of pages
    # pageobj = pdfreader.getPage(x-1)
    for page_number in range(x):
        page = pdfreader.getPage(page_number)
        text_1 = page.extractText()
        text += text_1
    # (x+1) because python indentation starts with 0.
    # create text variable which will store all text datafrom pdf file
    # text = pageobj.extractText()
    # save the extracted data from pdf to a txt file
    # we will use file handling here
    # dont forget to put r before you put the file path
    # go to the file location copy the path by right clicking on the file
    # click properties and copy the location path and paste it here.
    # put "\\your_txtfilename"
    new_file_name = file_name + '.txt'
    file1 = open(new_file_name, "a")
    file1.writelines(text)
    file1.close()
    return new_file_name


if __name__ == '__main__':
    app.run()
