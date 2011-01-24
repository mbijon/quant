import sys
import urllib

GOOGLE_URL = 'http://www.google.com/finance/historical?q={0}&output=csv'
EMA_N = 27 # days for EMA smoothing; EMA smoothing factor = 2 / (EMA_N + 1)


def rsi(ticker, ema_n = EMA_N):
	closes = google_close_prices(ticker)
	
	u_values, d_values = u_and_d_values(closes)
	
	u_emas = ema(u_values, ema_n)
	d_emas = ema(d_values, ema_n)
	
	rs_values = map(lambda u_ema, d_ema: u_ema / d_ema, u_emas, d_emas)
	rsi_values = [100 - 100 * (1 / (1 + rs)) for rs in rs_values]
	
	return rsi_values


def google_close_prices(ticker):
	url = GOOGLE_URL.format(ticker)
	f = urllib.urlopen(url)
	days = f.readlines()
	f.close()
	
	closes = [google_close_price(day) for day in days[1:]]
	closes.reverse()
	
	return closes


def google_close_price(day):
	return float(day.split(',')[4])


def u_and_d_values(close_prices):
	u_values = []
	d_values = []
	
	for i in range(1, len(close_prices)):
		close_today = close_prices[i]
		close_yesterday = close_prices[i - 1]
	
		u = max(close_today - close_yesterday, 0)
		d = max(close_yesterday - close_today, 0)
	
		u_values.append(u)
		d_values.append(d)
	
	return (u_values, d_values)


def ema(s, n):
	"""
	returns an n period exponential moving average for
	the time series s
	
	s is a list ordered from oldest (index 0) to most
	recent (index -1)
	n is an integer
	
	returns a numeric array of the exponential
	moving average
	
	from http://osdir.com/ml/python.matplotlib.general/2005-04/msg00044.html
	"""
	ema = []
	j = 1
	
	#get n sma first and calculate the next n period ema
	sma = sum(s[:n]) / n
	multiplier = 2 / float(1 + n)
	ema.append(sma)
	
	#EMA(current) = ( (Price(current) - EMA(prev) ) x Multiplier) + EMA(prev)
	ema.append(( (s[n] - sma) * multiplier) + sma)
	
	#now calculate the rest of the values
	for i in s[n+1:]:
		tmp = ( (i - ema[j]) * multiplier) + ema[j]
		j = j + 1
		ema.append(tmp)
	
	return ema


if __name__ == '__main__':
	ticker = 'AA'
	if len(sys.argv) > 1:
		ticker = sys.argv[1]
	
	rsi_values = rsi(ticker)
	print([int(rsi) for rsi in rsi_values])
	print('Latest RSI value for {0}: {1}'.format(ticker, rsi_values[-1]))
