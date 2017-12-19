import requests
import json
from bs4 import BeautifulSoup


class MeduzaNewsGrabber:
    CATEGORIES = {'news', 'cards', 'articles', 'shapito', 'polygon'}
    ENDPOINT = 'https://meduza.io/api/v3/'

    def __init__(self, category, locale='ru'):
        if category not in self.CATEGORIES:
            raise ValueError('Wrong category')
        self._category = category
        self._locale = locale

    def get_news_urls(self, page=0, per_page=50):
        payload = {
            'chrono': self._category,
            'locale': self._locale,
            'page': page,
            'per_page': per_page
        }
        response = requests.get(self.ENDPOINT + 'search/', params=payload)
        response.raise_for_status()
        documents = json.loads(response.text)['documents']
        return [news['url'] for news in documents.values()]

    def get_news_by_url(self, url):
        url = self.ENDPOINT + url
        response = requests.get(url)
        response.raise_for_status()
        body = json.loads(response.text)['root']['content']['body']
        return BeautifulSoup(body, 'html.parser').get_text().strip()


if __name__ == '__main__':
    from tqdm import tqdm
    from os.path import join

    path_output = 'news'
    grabber = MeduzaNewsGrabber('news')
    for idx, news_url in enumerate(tqdm(grabber.get_news_urls())):
        text = grabber.get_news_by_url(news_url)
        with open(join(path_output, f'news{idx}.txt'), 'w') as fout:
            fout.write(text)
