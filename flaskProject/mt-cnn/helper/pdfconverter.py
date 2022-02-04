# this python file is used to convert pdf to txt document
import os
import PyPDF2


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
    # loop through the pages and place the contents in text
    for page_number in range(x):
        page = pdfreader.getPage(page_number)
        # save the extracted data from pdf to a txt file
        text_1 = page.extractText()
        text += text_1

    # create a new file with txt extension
    new_file_name = file_name + '.txt'
    file1 = open(new_file_name, "a")
    file1.writelines(text)
    file1.close()
    return new_file_name
