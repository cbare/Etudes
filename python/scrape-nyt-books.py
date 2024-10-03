"""
Cooked up by GPT4o to scrape the New York Times Best Books of the 21st Century page
"""
import requests
from bs4 import BeautifulSoup
import json

# URL of the page
url = 'https://www.nytimes.com/interactive/2024/books/best-books-21st-century.html'

# Send a request to get the page content
response = requests.get(url)
soup = BeautifulSoup(response.content, 'html.parser')

# Find the section that contains the books
book_sections = soup.find('div', id='all-books').find_all('section')

def extract(section, *args, **kwargs):
    x = section.find(*args, **kwargs)
    return x.text.strip() if x else None


books = []
i = 100
for section in book_sections:
    print(i)
    title = section.find('h2').text.strip()
    author = extract(section, 'span', class_='author')
    author, _, translator = author.partition('; translated by ')
    year = extract(section, 'span', class_='year')
    description = extract(section, 'p', class_='g-text')
    image_url = section.find('img')['src']

    # Add the book details to the list
    books.append({
        'rank': i,
        'title': title,
        'author': author,
        'year': year,
        'description': description,
        'image_url': image_url
    })
    i -= 1
 
# Save the list as a JSON document
with open('books.json', 'w') as f:
    json.dump(books, f, indent=4)

print("Books data saved to books.json")
