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
		z = 0;
		m = 0;
		x = 0;
		ha = 1;
		alpha = 20;
		c = 0.25;
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	gear(int z, double m, double x = 0, double ha = 1, double alpha = 20, double c = 0.25)
	{
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	void Init(int z_in, double m_in, double x_in = 0, double ha_in = 1, double alpha_in = 20, double c_in = 0.25)
	{
		z = z_in;
		m = m_in;
		x = x_in;
		ha = ha_in;
		alpha = alpha_in;
		c = c_in;
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
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
		z = g_in.getZ();
		m = g_in.getM();
		x = g_in.getX();
		ha = g_in.getHa();
		alpha = g_in.getAlpha();
		c = g_in.getC();
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
	}
	void operator=(int z_in)
	{
		Init(z_in, 0);
	}
	gear& operator+(const int i)
	{
		z += i;
		r = m * z / 2.0;
		rb = r * cos(alpha*M_PI / 180.0);
		return *this;
	}

	~gear() {}
};

int pruning_zmin(const gear& g);
bool neighbourhood(const gear& gear2, const gear& gear3, int k = 1);

// LOL

class gearconnection
{
private:
	double alpha_w;
	double rw1;
	double rw2;
	double aw;		// межосевое расстояние

public:
	int sign;		// +1 - внутреннее зацепление -1 - внешнее зацепление
	double U12;		// передаточное отношение
	gear g1;
	gear g2;

gearconnection(gear& g1_in, gear& g2_in, int sign_in) 
{
	sign = sign_in;
	g1 = g1_in;
	g2 = g2_in;
	if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()))
	{
		double alpha = g1.getAlpha();
		double m = g1.getM();
		double inv_alpha = rad_to_inv(alpha);
		double x1 = g1.getX();
		double x2 = g2.getX();
		double z1 = g1.getZ();
		double z2 = g2.getZ();
		alpha_w = inv_to_rad(inv_alpha + 2*(x1 + x2)/(z1 + z2) * tan(alpha));
		rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
		rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
		aw = rw1 + rw2;
		U12 = sign * rw2 / rw1;
	}
	else 
	{
		cout << endl << "Ошибка (соединение с различными alpha или m!)" << endl;
	}
}
void recount()   // всегда использовать, после изменения параметров зубьев в передаче
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
		alpha_w = inv_to_rad(inv_alpha + 2 * (x1 + x2) / (z1 + z2) * tan(alpha));
		rw1 = m * z1 / 2 * cos(alpha) / cos(alpha_w);
		rw2 = m * z2 / 2 * cos(alpha) / cos(alpha_w);
		aw = rw1 + rw2;
		U12 = sign * rw2 / rw1;
	}
}
double getSign() const
{
	return sign;
}
double getU12() const
{
	return U12;
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

~gearconnection() {};
};

bool gearratio(const gearconnection& gConnection, double U12_theoretic, double error);

class planear_tr  // planear transmission - планетарная передача
{
public:
	int type;		// 1 - Однорядная, 2 - Двухрядная со смешанным зацеплением, 3 - Двухрядная с двумя внутренними зацеплениями, 4 - Двухрядная с 2мя внешними зацеплениями 
	int k;
	gear g1;
	gear g2;
	gear g3;
	gear g4;

	planear_tr(int type_in, int k_in, gear g1_in, gear g2_in, gear g3_in, gear g4_in)
	{
		if (type_in == 1)
		{
			type = type_in;
			k = k_in;
			g1 = g1_in;
			g2 = g2_in;
			g3 = g3_in;
			g4 = 0;
		}
		if ((type_in > 2) && (type_in <= 2))
		{
			type = type_in;
			k = k_in;
			g1 = g1_in;
			g2 = g2_in;
			g3 = g3_in;
			g4 = g4_in;
		}
	}
	
	int getType() const
	{
		return type;
	}
	int getK() const
	{
		return k;
	}

	~planear_tr() {}
};

bool alignment(planear_tr& PlTr);

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
bool gearratio(const gearconnection& gConnection, double U12_theoretic, double error = 5)      // error - погрешность. сравниваем первое передаточное отношение со вторым (должно выполняться U1 = U2*(1-погрешность) .... U2*(1+погрешность))
{
	bool temp;
	if ((gConnection.getU12() >= ((1 - error / 100) * U12_theoretic)) && (gConnection.getU12() <= ((1 + error / 100) * U12_theoretic)))
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
// Нахождение угла инволюты
double inv_to_rad(double Inv) 
{
	double temp = pow((3.0 * Inv), double(1.0 / 3.0)) - (2.0 * Inv) / 5.0 + double(9.0 / 175.0)*pow(3.0, double(2.0 / 3.0))*pow(Inv, double(5.0 / 3.0)) - double(2.0/175.0)*pow(3.0, double(1.0/3.0))*pow(Inv, double(7.0/3.0)) - double(144.0/67375.0)*pow(Inv, 3.0) + double(3258.0/3128125.0)*pow(3.0, double(2.0/3.0))*pow(Inv, double(11.0/3.0)) - double(49711.0/153278125.0)*pow(3.0, double(1.0/3.0))*pow(Inv, double(13.0/3.0));
	return temp;
}
// 4 Условие (Соосность)
bool alignment(planear_tr& PlTr)	// соосность aw1 = aw2. m1 - модуль первого ряда. m2 - модуль второго ряда
{
	if (PlTr.getType() == 1)	// 1 - Однорядная 
	{
		gearconnection gConnection1(PlTr.g1, PlTr.g2, -1);
		gearconnection gConnection2(PlTr.g2, PlTr.g3, 1);
		double aw1 = gConnection1.getAw();
		double aw2 = gConnection2.getAw();
		if (aw1 == aw2) {
			return 1;
		}
		else { 
			return 0; 
		}
	}
	if (PlTr.getType() == 2)	// 2 - Двухрядная со смешанным зацеплением
	{
		gearconnection gConnection1(PlTr.g1, PlTr.g2, -1);
		gearconnection gConnection2(PlTr.g3, PlTr.g4, 1);
		double aw1 = gConnection1.getAw();
		double aw2 = gConnection2.getAw();
		if (aw1 == aw2) {
			return 1;
		}
		else {
			return 0;
		}
	}
	if (PlTr.getType() == 3)	// 3 - Двухрядная с 2мя внутренними зацеплениями
	{
		gearconnection gConnection1(PlTr.g1, PlTr.g2, 1);
		gearconnection gConnection2(PlTr.g3, PlTr.g4, 1);
		double aw1 = gConnection1.getAw();
		double aw2 = gConnection2.getAw();
		if (aw1 == aw2) {
			return 1;
		}
		else {
			return 0;
		}
	}
	if (PlTr.getType() == 4)	// 4 - Двухрядная с 2мя внешними зацеплениями
	{
		gearconnection gConnection1(PlTr.g1, PlTr.g2, -1);
		gearconnection gConnection2(PlTr.g3, PlTr.g4, -1);
		double aw1 = gConnection1.getAw();
		double aw2 = gConnection2.getAw();
		if (aw1 == aw2) {
			return 1;
		}
		else {
			return 0;
		}
	}
	cout << endl << "Ошибка (соосность)!" << endl;
	return 0;
}
// 5 Условие (Соседство)
bool neighbourhood(const planear_tr& PlTr)
{

	return 0;
}

double rad_to_inv(double angle)  // радианы в инволюту
{
	return (tan(angle) - angle);
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
	return;
}
