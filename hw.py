import sys
import urllib
import datetime


GOOGLE_URL = 'http://www.google.com/finance/historical?q={0}&output=csv' # past year
GOOGLE_URL = 'http://www.google.com/finance/historical?q={0}&startdate={1}+{2}%2C+{3}&enddate={1}+{2}%2C+{4}&output=csv'


def hw_optimal_mape_and_parameters(data, resolution_depth = 2, preset_season_length = None):
	best_mape = None
	best_parameters = (0, 0, 0, 0)
	
	if preset_season_length != None:
		season_length_range = [preset_season_length]
	
	if resolution_depth == None: resolution_depth = 2
	resolutions = [10 ** -(index + 1) for index in range(0, resolution_depth)]
	
	for resolution in resolutions:
		alpha, beta, gamma, season_length = best_parameters
		
		alpha_range = [abs(i * resolution - 5 * resolution + alpha) for i in range(10)]
		beta_range = [abs(i * resolution - 5 * resolution + beta) for i in range(10)]
		gamma_range = [abs(i * resolution - 5 * resolution + gamma) for i in range(10)]
		
		if preset_season_length == None:
			season_length_range = [int(abs(i * (resolution * 10000) - 5 * (resolution * 10000) + season_length)) for i in range(10)]
		
		for alpha in alpha_range:
			for beta in beta_range:
				for gamma in gamma_range:
					for season_length in season_length_range:
						if season_length == 0: season_length = 1
						
						parameters = alpha, beta, gamma, season_length
						mape = hw_mape(data, parameters)
						
						if mape != None and (best_mape == None or mape < best_mape):
							best_mape = mape
							best_parameters = parameters
	
	return best_mape, best_parameters


def hw_mape(data, parameters):
	alpha, beta, gamma, season_length = parameters
	
	if len(data) < 3 * season_length or season_length == 0:
		return None
	
	calculation_data = data[:-season_length]
	testing_data = data[-season_length:]
	
	r, g, s_values = hw(data, parameters)
	
	pe_sum = 0
	for index in range(len(testing_data)):
		datum = testing_data[index]
		predicted_datum = (r + index * g) * s_values[-index + season_length]
		#print '({0:6.1f} + {1:6.1f}) * {2:4.2f} = {3:6.1f} ~ {4:6.1f} ({5:6.4f})'.format(r, g, s_values[l], predicted_datum, datum, percent_delta)
		
		percentage_error = abs(1 - predicted_datum / datum )
		pe_sum += percentage_error
	
	if pe_sum == 0:
		return None
	
	return pe_sum / season_length

def hw(data, parameters):
	alpha, beta, gamma, season_length = parameters
	
	prev_r = data[0] # R_t-1
	r = data[1] # R_t
	
	prev_g = data[1] - data[0] # G_t-1
	g = data[2] - data[1] # G_t
	
	s_values = [1] * (season_length + 1) # s_values[l] = S_t-l, s_values[0] = S_t
	
	for datum in data:
		r, prev_r = (alpha * (datum / s_values[season_length - 1])) + ((1 - alpha) * (prev_r + prev_g)), r
		s_values = [(gamma * (datum / r)) + ((1 - gamma) * s_values[season_length])] + s_values[:-1]
		g, prev_g = (beta * (s_values[0] - s_values[-1])) + ((1 - beta) * prev_g), g
	
	return r, g, s_values;


def google_close_prices(ticker, start_year = None):
	today = datetime.date.today()
	if start_year == None:
		start_year = today.year - 2
	
	url = GOOGLE_URL.format(ticker, today.strftime('%b'), today.day, start_year, today.year)
	f = urllib.urlopen(url)
	days = f.readlines()
	f.close()

	closes = [google_close_price(day) for day in days[1:]]
	closes.reverse() # earliest = closes[0], latest = closes[-1]

	return closes


def google_close_price(day):
	return float(day.split(',')[4])


if __name__ == '__main__':
	ticker = 'AA'
	if len(sys.argv) > 1:
		ticker = sys.argv[1]
	
	start_year = None
	if len(sys.argv) > 2:
		start_year = int(sys.argv[2])
	
	resolution_depth = None
	if len(sys.argv) > 3:
		resolution_depth = int(sys.argv[3])
	
	season_length = None
	if len(sys.argv) > 4:
		season_length = int(sys.argv[4])

	data = google_close_prices(ticker, start_year)
	mape, parameters = hw_optimal_mape_and_parameters(data, resolution_depth, season_length)
	
	if mape == None:
		print "There was an error; sorry. :( Probably there isn't enough data for the given season lengths."
		sys.exit()
	
	print '{0:>14} : {1}'.format('mape', mape)
	print '{0:>14} : {1}'.format('alpha', parameters[0])
	print '{0:>14} : {1}'.format('beta', parameters[1])
	print '{0:>14} : {1}'.format('gamma', parameters[2])
	print '{0:>14} : {1}'.format('season length', parameters[3])
