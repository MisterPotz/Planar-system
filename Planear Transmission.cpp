#include <iostream>
#include <math.h>
using namespace std;

#define M_PI 3.141592653589793238462643

bool gearratio(double U12, double U12_theoretic, double error);
int pruning_zmin(double x, double ha, double alpha);
double inv_to_rad(double Inv);
double rad_to_inv(double angle);

class gear
{
public:
	int gsign;		// +1 - внешнее колесо, 0 - не существующее колесо, -1 - внутреннее колесо 
	int z;
	double m;
	double x;
	double ha;
	double alpha;
	double c;

private:
	double r;
	double rb;

public:
	gear()
	{
		gsign = 0;
		z = 0;
		m = 0;
		x = 0;
		ha = 1;
		alpha = 20;
		c = 0.25;
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	gear(int gsign_in, int z_in, double m_in, double x_in = 0, double ha_in = 1, double alpha_in = 20, double c_in = 0.25)
	{
		gsign = gsign_in;
		z = z_in;
		m = m_in;
		x = x_in;
		ha = ha_in;
		alpha = alpha_in;
		c = c_in;
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	void Init(int gsign_in, int z_in, double m_in, double x_in = 0, double ha_in = 1, double alpha_in = 20, double c_in = 0.25)
	{
		gsign = gsign_in;
		z = z_in;
		m = m_in;
		x = x_in;
		ha = ha_in;
		alpha = alpha_in;
		c = c_in;
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	int getGsign() const
	{
		return gsign;
	}
	int getZ() const
	{
		return z;
	}
	double getM() const
	{
		return m;
	}
	double getX() const
	{
		return x;
	}
	double getHa() const
	{
		return ha;
	}
	double getAlpha() const
	{
		return alpha;
	}
	double getR() const
	{
		return r;
	}
	double getRb() const
	{
		return rb;
	}
	double getC() const
	{
		return c;
	}

	void operator=(const gear& g_in)
	{
		gsign = g_in.getGsign();
		z = g_in.getZ();
		m = g_in.getM();
		x = g_in.getX();
		ha = g_in.getHa();
		alpha = g_in.getAlpha();
		c = g_in.getC();
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	gear& operator+=(const int i)
	{
		if (gsign != 0)
		{
			z += i;
			r = m * z / 2.0;
			rb = r * cos(alpha*M_PI / 180.0);
		}
		return *this;
	}

	~gear() {}
};

int pruning_zmin(const gear& g);
bool neighbourhood(const gear& gear2, const gear& gear3, int k = 1);

// LOL

class gear_connection
{
private:
	double alpha_w;
	double rw1;
	double rw2;
	double aw;		// межосевое расстояние
	int sign;		// +1 - внутреннее зацепление -1 - внешнее зацепление

public:
	double U;		// передаточное отношение
	gear g1;
	gear g2;

	gear_connection(){}
	gear_connection(gear& g1_in, gear& g2_in)
	{
		g1 = g1_in;
		g2 = g2_in;
		int g1sign = g1.getGsign();
		int g2sign = g2.getGsign();
		if (((g1sign == -1) && (g2sign == -1)) || ((g1sign == 0) || (g2sign == 0)))			// Проверка на дурачка (недопустимы два внутренних колеса и недопустимы пустые колеса)
		{
			cout << endl << "Ошибка! (недопустимые колеса в зацеплении)" << endl;
		}
		sign = (-1) * g1sign * g2sign;
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()))
		{
			double alpha = g1.getAlpha();
			double m = g1.getM();
			double inv_alpha = rad_to_inv(alpha);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();

			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_w = inv_to_rad(inv_alpha + 2 * (x1 + x2) / (z1 + z2) * tan(alpha));
				rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
				rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x2 - x1) / (z2 - z1) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x1 - x2) / (z1 - z2) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw1 - rw2;
				}
			}
			U = sign * rw2 / rw1;
		}
		else
		{
			cout << endl << "Ошибка (соединение с различными alpha или m!)" << endl;
		}
	}

	void Init(gear& g1_in, gear& g2_in)
	{
		g1 = g1_in;
		g2 = g2_in;
		int g1sign = g1.getGsign();
		int g2sign = g2.getGsign();
		if (((g1sign == -1) && (g2sign == -1)) || ((g1sign == 0) || (g2sign == 0)))			// Проверка на дурачка (недопустимы два внутренних колеса и недопустимы пустые колеса)
		{
			cout << endl << "Ошибка! (недопустимые колеса в зацеплении)" << endl;
		}
		sign = (-1) * g1sign * g2sign;
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()))
		{
			double alpha = g1.getAlpha();
			double m = g1.getM();
			double inv_alpha = rad_to_inv(alpha);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();
			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_w = inv_to_rad(inv_alpha + 2 * (x1 + x2) / (z1 + z2) * tan(alpha));
				rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
				rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x2 - x1) / (z2 - z1) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x1 - x2) / (z1 - z2) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw1 - rw2;
				}
			}
			U = sign * rw2 / rw1;
		}
		else 
		{
			cout << endl << "Разные alpha или m при инициализации в зацеплении" << endl;
		}
	}

	void recount()	   // всегда использовать, после изменения параметров зубьев в передаче
	{
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()))
		{
			double alpha = g1.getAlpha();
			double m = g1.getM();
			double inv_alpha = rad_to_inv(alpha);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();
			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_w = inv_to_rad(inv_alpha + 2 * (x1 + x2) / (z1 + z2) * tan(alpha));
				rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
				rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x2 - x1) / (z2 - z1) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x1 - x2) / (z1 - z2) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw1 - rw2;
				}
			}
			U = sign * rw2 / rw1;
		}
		else
		{
			cout << endl << "Разные alpha или m при инициализации в зацеплении" << endl;
		}
	}
	double getSign() const
	{
		return sign;
	}
	double getU() const
	{
		return U;
	}
	double getRevU() const
	{
		return pow(U,-1.0);
	}
	double getAlpha_w() const
	{
		return alpha_w;
	}
	double getRw1() const
	{
		return rw1;
	}
	double getRw2() const
	{
		return rw2;
	}
	double getAw() const
	{
		return aw;
	}

	void operator=(const gear_connection& gc_in)
	{
		g1 = gc_in.g1;
		g2 = gc_in.g2;
		int g1sign = g1.getGsign();
		int g2sign = g2.getGsign();
		if (((g1sign == -1) && (g2sign == -1)) || ((g1sign == 0) || (g2sign == 0)))			// Проверка на дурачка (недопустимы два внутренних колеса и недопустимы пустые колеса)
		{
			cout << endl << "Ошибка! (недопустимые колеса в зацеплении)" << endl;
		}
		sign = (-1) * g1sign * g2sign;
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()))
		{
			double alpha = g1.getAlpha();
			double m = g1.getM();
			double inv_alpha = rad_to_inv(alpha);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();
			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_w = inv_to_rad(inv_alpha + 2 * (x1 + x2) / (z1 + z2) * tan(alpha));
				rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
				rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x2 - x1) / (z2 - z1) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_w = inv_to_rad(inv_alpha + 2 * (x1 - x2) / (z1 - z2) * tan(alpha));
					rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
					rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
					aw = rw1 - rw2;
				}
			}
			U = sign * rw2 / rw1;
		}
		else
		{
			cout << endl << "Ошибка (соединение с различными alpha или m!)" << endl;
		}
	}

	~gear_connection() {};
};

bool gearratio(const gear_connection& gConnection, double U12_theoretic, double error);

class planetary_transmission  // planetary transmission - планетарная передача
{
public:
	int type;		// 1 - Однорядная, 2 - Двухрядная со смешанным зацеплением, 3 - Двухрядная с двумя внутренними зацеплениями, 4 - Двухрядная с 2мя внешними зацеплениями 
	int k;
	gear_connection gc1;
	gear_connection gc2;

	planetary_transmission(int type_in, int k_in, gear g1_in, gear g2_in, gear g3_in, gear g4_in)
	{
			type = type_in;
			k = k_in;
			gc1.Init(g1_in, g2_in);
			gc2.Init(g3_in, g4_in);
	}

	gear_connection& getGc1() 
	{
		return gc1;
	}
	gear_connection& getGc2()
	{
		return gc2;
	}

	int getType() const
	{
		return type;
	}
	int getK() const
	{
		return k;
	}

	~planetary_transmission() {}
};

bool alignment(planetary_transmission& PlTr);

// радианы в инволюту
double rad_to_inv(double angle)  
{
	return (tan(angle) - angle);
}
// Нахождение угла инволюты
double inv_to_rad(double Inv)
{
	double temp = pow((3.0 * Inv), double(1.0 / 3.0)) - (2.0 * Inv) / 5.0 + double(9.0 / 175.0)*pow(3.0, double(2.0 / 3.0))*pow(Inv, double(5.0 / 3.0)) - double(2.0 / 175.0)*pow(3.0, double(1.0 / 3.0))*pow(Inv, double(7.0 / 3.0)) - double(144.0 / 67375.0)*pow(Inv, 3.0) + double(3258.0 / 3128125.0)*pow(3.0, double(2.0 / 3.0))*pow(Inv, double(11.0 / 3.0)) - double(49711.0 / 153278125.0)*pow(3.0, double(1.0 / 3.0))*pow(Inv, double(13.0 / 3.0));
	return temp;
}

// 1 Условие (Передаточное отношение)
bool gearratio(double U12, double U12_theoretic, double error = 5)      // error - погрешность. сравниваем первое передаточное отношение со вторым (должно выполняться U1 = U2*(1-погрешность) .... U2*(1+погрешность))
{
	bool temp;
	if ((U12 >= ((1 - error / 100) * U12_theoretic)) && (U12 <= ((1 + error / 100) * U12_theoretic)))
	{
		temp = true;
	}
	else
	{
		temp = false;
	}
	return temp;
}
bool gearratio(const gear_connection& gConnection, double U12_theoretic, double error = 5)      // error - погрешность. сравниваем первое передаточное отношение со вторым (должно выполняться U1 = U2*(1-погрешность) .... U2*(1+погрешность))
{
	bool temp;
	if ((gConnection.getU() >= ((1 - error / 100) * U12_theoretic)) && (gConnection.getU() <= ((1 + error / 100) * U12_theoretic)))
	{
		temp = true;
	}
	else
	{
		temp = false;
	}
	return temp;
}
// 2 Условие (Подрезание)
int pruning_zmin(double x, double ha = 1, double alpha = 20)		// определяет минимальное число z без подрезания. x - смещение, h*a - коэффициент высоты зуба, alpha - угол профиля
{
	double radiansalpha = alpha * double(M_PI) / 180.0;
	double z_temp = 2 * (ha - x) / pow(sin(radiansalpha), 2.0);
	return ceil(z_temp);		//ceil() округляет в сторону большего
}
int pruning_zmin(const gear& g)		// определяет минимальное число z без подрезания. x - смещение, h*a - коэффициент высоты зуба, alpha - угол профиля
{
	double radiansalpha = g.getAlpha() * double(M_PI) / 180.0;
	double z_temp = 2 * (g.getHa() - g.getX()) / pow(sin(radiansalpha), 2.0);
	return ceil(z_temp);		//ceil() округляет в сторону большего
}
// 4 Условие (Соосность)
bool alignment(planetary_transmission& PlTr)	// соосность aw1 = aw2 +- 0.05
{
	gear_connection gConnection1 = PlTr.getGc1();
	gear_connection gConnection2 = PlTr.getGc2();
	double aw1 = gConnection1.getAw();
	double aw2 = gConnection2.getAw();
	if ((aw1 > aw2 - 0.05) && (aw1 < aw2 + 0.05))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

// 5 Условие (Соседство)
bool neighbourhood(const planetary_transmission& PlTr)
{

	return 0;
}


void main()
{
	double U1 = 98.9;
	double U2 = 100;
	cout << "True? : " << gearratio(U1, U2) << endl;
	cout << pruning_zmin(0.5) << endl;
	cout << inv_to_rad(1) << " - " << rad_to_inv(inv_to_rad(1)) << endl;
	cout << inv_to_rad(0.8) << " - " << rad_to_inv(inv_to_rad(0.8)) << endl;
	cout << inv_to_rad(0.6) << " - " << rad_to_inv(inv_to_rad(0.6)) << endl;
	cout << inv_to_rad(0.4) << " - " << rad_to_inv(inv_to_rad(0.4)) << endl;
	cout << inv_to_rad(0.2) << " - " << rad_to_inv(inv_to_rad(0.2)) << endl;
	cout << inv_to_rad(0) << endl;

	system("PAUSE");
}
