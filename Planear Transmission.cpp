#include <iostream>
#include <math.h>
using namespace std;

#define M_PI 3.141592653589793238462643

bool gearratio(double U, double U_theoretic, double error);
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
	double alpha;	// градусы
	double c;

private:
	double r;
	double rb;
	// для общего случая (включая коэффициент бэта (косозубые передачи)) :
	double beta; // в радианах
	double mt;			
	double hta;
	double ct;
	double alpha_t; // в радианах
	double xt;
public:
	gear()
	{
		gsign = 0;
		z = 0;
		m = 0;
		x = 0;
		ha = 1;
		alpha = 20; 
		beta = 0;
		c = 0.25;
		
		mt = m / cos(beta);
		hta = ha * cos(beta);
		ct = c * cos(beta);
		alpha_t = atan( tan(alpha * M_PI / 180) / cos(beta) );
		xt = x * cos(beta);
		r = m * z / 2.0 / cos(beta);
		rb = r * cos(alpha_t);
	}
	gear(int gsign_in, int z_in, double m_in, double x_in = 0, double alpha_in = 20, double beta_in = 0, double ha_in = 1, double c_in = 0.25)
	{
		gsign = gsign_in;
		z = z_in;
		m = m_in;
		x = x_in;
		alpha = alpha_in;
		beta = beta_in * M_PI / 180;
		ha = ha_in;
		c = c_in;

		mt = m / cos(beta);
		hta = ha * cos(beta);
		ct = c * cos(beta);
		alpha_t = atan(tan(alpha * M_PI / 180) / cos(beta));
		xt = x * cos(beta);
		r = m * z / 2.0 / cos(beta);
		rb = r * cos(alpha_t);
	}
	void Init(int gsign_in, int z_in, double m_in, double x_in = 0, double alpha_in = 20, double beta_in = 0, double ha_in = 1, double c_in = 0.25)
	{
		gsign = gsign_in;
		z = z_in;
		m = m_in;
		x = x_in;
		ha = ha_in;
		alpha = alpha_in;
		beta = beta_in * M_PI / 180;
		c = c_in;
		
		mt = m / cos(beta);
		hta = ha * cos(beta);
		ct = c * cos(beta);
		alpha_t = atan(tan(alpha * M_PI / 180) / cos(beta));
		xt = x * cos(beta);
		r = m * z / 2.0 / cos(beta);
		rb = r * cos(alpha_t);
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
	double getBeta() const
	{
		return beta;
	}
	double getC() const
	{
		return c;
	}
	double getR() const
	{
		return r;
	}
	double getRb() const
	{
		return rb;
	}
	double getMt() const
	{
		return mt;
	}
	double getHta() const
	{
		return hta;
	}
	double getCt() const
	{
		return ct;
	}
	double getAlpha_t() const
	{
		return alpha_t;
	}
	double getXt() const
	{
		return xt;
	}
	
	void print() const
	{
		cout << "Число gsign =			" << getGsign() << endl;
		cout << "Число z =			" << getZ()		<< endl;
		cout << "Число m =			" << getM()		<< endl;
		cout << "Число x =			" << getX()		<< endl;
		cout << "Число h*a =			" << getHa()	<< endl;
		cout << "Число alpha =			" << getAlpha() << endl;
		cout << "Число r =			" << getR()		<< endl;
		cout << "Число rb =			" << getRb()	<< endl;
		cout << "Число c* =			" << getC()		<< endl;
		cout << "Число beta =			" << getBeta() << endl;
	}

	void operator=(const gear& g_in)
	{
		gsign = g_in.getGsign();
		z = g_in.getZ();
		m = g_in.getM();
		x = g_in.getX();
		ha = g_in.getHa();
		alpha = g_in.getAlpha();
		beta = g_in.getBeta();
		c = g_in.getC();

		mt = m / cos(beta);
		hta = ha * cos(beta);
		ct = c * cos(beta);
		alpha_t = atan(tan(alpha * M_PI / 180) / cos(beta));
		xt = x * cos(beta);
		r = m * z / 2.0 / cos(beta);
		rb = r * cos(alpha_t);
	}
	gear& operator+=(const int i)
	{
		if (gsign != 0)
		{
			z += i;
			r = m * z / 2.0 / cos(beta);
			rb = r * cos(alpha_t);
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
	int sign;		// +1 - внутреннее зацепление -1 - внешнее зацепление
	double U;		// передаточное отношение
	double rw1;
	double rw2;
	double aw;		// межосевое расстояние

	double alpha_tw; // радианы
public:
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
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()) && (g1.getBeta() == g2.getBeta()))
		{
			double alpha = g1.getAlpha();
			double alpha_t = g1.getAlpha_t();
			double m = g1.getM();
			double beta = g1.getBeta();
			double inv_alpha_t = rad_to_inv(alpha_t);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();

			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 + x2) / (z1 + z2) * tan(alpha * M_PI / 180));
				rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x2 - x1) / (z2 - z1) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 - x2) / (z1 - z2) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					aw = rw1 - rw2;
				}
			}
			U = sign * rw2 / rw1;
		}
		else
		{
			cout << endl << "Ошибка (соединение с различными alpha, beta или m!)" << endl;
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
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()) && (g1.getBeta() == g2.getBeta()))
		{
			double alpha = g1.getAlpha();
			double alpha_t = g1.getAlpha_t();
			double m = g1.getM();
			double beta = g1.getBeta();
			double inv_alpha_t = rad_to_inv(alpha_t);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();

			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 + x2) / (z1 + z2) * tan(alpha * M_PI / 180));
				rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x2 - x1) / (z2 - z1) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 - x2) / (z1 - z2) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
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

	void recountGC()	   // всегда использовать, после изменения параметров зубьев в передаче
	{
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()) && (g1.getBeta() == g2.getBeta()))
		{
			double alpha = g1.getAlpha();
			double alpha_t = g1.getAlpha_t();
			double m = g1.getM();
			double beta = g1.getBeta();
			double inv_alpha_t = rad_to_inv(alpha_t);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();

			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 + x2) / (z1 + z2) * tan(alpha * M_PI / 180));
				rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x2 - x1) / (z2 - z1) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 - x2) / (z1 - z2) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
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
	double getAlpha_tw() const
	{
		return alpha_tw;
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
	int getZ1() const 
	{
		return g1.getZ();
	}
	int getZ2() const
	{
		return g2.getZ();
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
		if ((g1.getAlpha() == g2.getAlpha()) && (g1.getM() == g2.getM()) && (g1.getBeta() == g2.getBeta()))
		{
			double alpha = g1.getAlpha();
			double alpha_t = g1.getAlpha_t();
			double m = g1.getM();
			double beta = g1.getBeta();
			double inv_alpha_t = rad_to_inv(alpha_t);
			double x1 = g1.getX();
			double x2 = g2.getX();
			double z1 = g1.getZ();
			double z2 = g2.getZ();

			if (sign == -1)		// -1 - это внешнее зацепление
			{
				alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 + x2) / (z1 + z2) * tan(alpha * M_PI / 180));
				rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
				aw = rw1 + rw2;
			}
			else	// внутреннее зацепление
			{
				if (g1.getGsign() == 1)		// первое колесо - внешнее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x2 - x1) / (z2 - z1) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					aw = rw2 - rw1;
				}
				else	// первое колесо - внутреннее
				{
					alpha_tw = inv_to_rad(inv_alpha_t + 2 * (x1 - x2) / (z1 - z2) * tan(alpha * M_PI / 180));
					rw1 = m * z1 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
					rw2 = m * z2 / 2 / cos(beta) * cos(alpha_t) / cos(alpha_tw);
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
	double U1k_h;
	gear_connection gc1;
	gear_connection gc2;

	planetary_transmission(int type_in, int k_in, gear_connection gc1_in, gear_connection gc2_in)
	{
			type = type_in;
			k = k_in;
			gc1 = gc1_in;
			gc2 = gc2_in;
			U1k_h = gc1.getU() * gc2.getU();
	}

	planetary_transmission()
	{
		type = 0;	// тип 0 - это пустая передача
	}

	void InitPT(int type_in, int k_in, gear_connection gc1_in, gear_connection gc2_in)
	{
		type = type_in;
		k = k_in;
		gc1 = gc1_in;
		gc2 = gc2_in;
		U1k_h = gc1.getU() * gc2.getU();
	}

	const gear_connection& getGc1() const 
	{
		return gc1;
	}
	const gear_connection& getGc2() const
	{
		return gc2;
	}

	void recountPT() 
	{
		gc1.recountGC();
		gc2.recountGC();
		U1k_h = gc1.getU() * gc2.getU();
	}
	int getType() const
	{
		return type;
	}
	int getK() const
	{
		return k;
	}
	double getU1k_h() const
	{
		return U1k_h;
	}
	double getUk1_h() const
	{
		return pow(U1k_h, (-1.0));
	}
	double getU1h_k() const
	{
		return (1 - getU1k_h());
	}
	double getUh1_k() const
	{
		return pow(getU1h_k(), (-1.0));
	}
	int getZ1() const 
	{
		return gc1.getZ1();
	}
	int getZ2() const
	{
		return gc1.getZ2();
	}
	int getZ3() const
	{
		return gc2.getZ1();
	}

	void operator=(const planetary_transmission& PT_in) 
	{
		type = PT_in.getType();
		k = PT_in.getK();
		gc1 = PT_in.getGc1();
		gc2 = PT_in.getGc2();
		U1k_h = PT_in.gc1.getU() * PT_in.gc2.getU();
	}

	~planetary_transmission() {}
};

bool alignment(planetary_transmission& PlTr);
double maxRw(const planetary_transmission PlTr);

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
bool gearratio(const planetary_transmission& PlTr, double U_theoretic, double error = 5)      // error - погрешность. сравниваем первое передаточное отношение со вторым (должно выполняться U1 = U2*(1-погрешность) .... U2*(1+погрешность))
{
	double U;
	double Array [4] = {PlTr.getU1h_k(), PlTr.getUh1_k(), PlTr.getU1k_h(), PlTr.getUk1_h() };
	
	for (int i = 0; i < 4; i++) 
	{
		U = Array[i];
		if (U >= 0) 
		{
			if ((U >= ((1 - error / 100) * U_theoretic)) && (U <= ((1 + error / 100) * U_theoretic)))
			{
				return true;
			}
		}
		else 
		{
			if ((U >= ((1 + error / 100) * U_theoretic)) && (U <= ((1 - error / 100) * U_theoretic)))
			{
				return true;
			}
		}
	}
	return false;		// 0, если не выполнено
}

// 2 Условие (Подрезание)
int pruning_zmin(const gear& g)		// определяет минимальное число z без подрезания. x - смещение, h*a - коэффициент высоты зуба, alpha - угол профиля
{
	double radiansalpha_t = g.getAlpha_t() * double(M_PI) / 180.0;
	double z_temp = 2 * (g.getHta() - g.getXt()) / pow(sin(radiansalpha_t), 2.0);
	return ceil(z_temp);		//ceil() округляет в сторону большего
}
bool pruning(const planetary_transmission& PlTr)		// совсем по уму, для гибкого кода, это должен быть цикл, который находит внешние колёса и чекает их Z c Zmin
{
	bool flag = true;
	if (PlTr.gc1.g1.getGsign() == 1)				// на подрезание проверяются только внешние колёса (1 колесо)
	{
		if (PlTr.gc1.g1.getZ() <= pruning_zmin(PlTr.gc1.g1))			// если z < zmin срубаем флаг
		{
			flag = false;
		}
	}
	if (PlTr.gc1.g2.getGsign() == 1)				// на подрезание проверяются только внешние колёса	(2 колесо)
	{
		if (PlTr.gc1.g2.getZ() <= pruning_zmin(PlTr.gc1.g2))			// если z < zmin срубаем флаг
		{
			flag = false;
		}
	}
	if (PlTr.gc2.g1.getGsign() == 1)				// на подрезание проверяются только внешние колёса	(3 колесо)
	{
		if (PlTr.gc2.g1.getZ() <= pruning_zmin(PlTr.gc2.g1))			// если z < zmin срубаем флаг
		{
			flag = false;
		}
	}
	if (PlTr.gc2.g2.getGsign() == 1)				// на подрезание проверяются только внешние колёса	(4 колесо)
	{
		if (PlTr.gc2.g2.getZ() <= pruning_zmin(PlTr.gc2.g2))			// если z < zmin срубаем флаг
		{
			flag = false;
		}
	}
	return flag;	// возвращает 0, если не выполняется условие!!!
}

// 3 Условие правильности внутреннего зацепления
bool checkIN(gear_connection& gc)		// принимает заведомо внутреннюю передачу 
{
	bool flag = true;

	// ПУНКТ А) условие отсутствия интерференции вершин зубьев

	double m = gc.g1.getM();		//одинаковый для обоих
	double r1;
	double ha1;
	double x1;
	double rb1;
	int z1;

	double r2;
	double ha2;
	double x2;
	double rb2;
	int z2;

	if (gc.g1.getGsign() == 1)									// 1 вариант - g1 = внешнее
	{
		r1 = gc.g1.getR();
		ha1 = gc.g1.getHa();
		x1 = gc.g1.getX();
		rb1 = gc.g1.getRb();
		z1 = gc.g1.getZ();

		r2 = gc.g2.getR();
		ha2 = gc.g2.getHa();
		x2 = gc.g2.getX();
		rb2 = gc.g2.getRb();
		z2 = gc.g2.getZ();
	}
	if (gc.g2.getGsign() == 1)									// 2 вариант - g2 = внешнее	(индексы g1 и g2 меняются местами)
	{
		r1 = gc.g2.getR();
		ha1 = gc.g2.getHa();
		x1 = gc.g2.getX();
		rb1 = gc.g2.getRb();
		z1 = gc.g2.getZ();

		r2 = gc.g1.getR();
		ha2 = gc.g1.getHa();
		x2 = gc.g1.getX();
		rb1 = gc.g1.getRb();
		z1 = gc.g1.getZ();
	}

	double alpha_tw = gc.getAlpha_tw();
	double aw = gc.getAw();

	// индекс 1 - для внешнего(шестерни), 2 - для внутреннего(колеса)
	double da1 = 2 * r1 + 2 * (ha1 + x1) * m;
	double da2 = 2 * r2 - 2 * (ha2 - x2 - 0.2) * m;
	double alpha_a1 = acos(2 * rb1 / da1);
	double alpha_a2 = acos(2 * rb2 / da2);
	double gamma12 = (double)z1 / z2 * rad_to_inv(alpha_a1) - rad_to_inv(alpha_a2) + (1 - ((double)z1 / z2)) * rad_to_inv(alpha_tw);
	double mu_max = acos((pow(da2, 2.0)) - (pow(da1, 2.0)) - 4 * (pow(aw, 2.0)) / (4 * aw * da1));
	double delta = (double)z1/z2 * mu_max - asin(da1/da2 * sin(mu_max)) + gamma12;

	if (delta < 0)				//delta должна быть больше или равна 0
	{
		flag = false;
	}

	// ПУНКТ Б) проверка коэффициента перекрытия
	
	double eps_alpha = (z1 * tan(alpha_a1) - z2 * tan(alpha_a2) + (z2 - z1) * tan(alpha_tw)) / (2 * M_PI);

	double tmp;
	if (gc.g1.getBeta() == 0)		
	{
		tmp = 1.2;		// для прямозубой передачи
	}
	else 
	{
		tmp = 1;		// для косозубой передачи
	}
	if (eps_alpha < tmp)		
	{
		flag = false;
	}

	return flag;		// возвращает 0, если не выполняется условие!!!
}
bool in_connection(planetary_transmission& PlTr)		// находим внутренние зацепы, проверяем (в идеале, в твоём коде - это цикл по внутренним зацплениям)  
{
	bool flag = true;
	if (PlTr.gc1.getSign() == 1)	// проверяем первую передачу (если она внутренняя) 
	{
		flag = checkIN(PlTr.gc1);
	}
	if (PlTr.gc2.getSign() == 1)		// проверяем вторую передачу (если она внутренняя)
	{
		if (flag != 0)
		flag = checkIN(PlTr.gc2);
	}
	return flag;		// возвращает 0, если не выполняется условие!!!
}

// 4 Условие (Соосность)
bool alignment(planetary_transmission& PlTr)	// соосность aw1 = aw2 +- 0.05
{
	gear_connection gConnection1 = PlTr.getGc1();
	gear_connection gConnection2 = PlTr.getGc2();
	double aw1 = gConnection1.getAw();
	double aw2 = gConnection2.getAw();
	if ((aw1 >= aw2 - 0.05) && (aw1 <= aw2 + 0.05))
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
	double A;
	int z1 = PlTr.getZ1();
	int z2 = PlTr.getZ2();
	int z3 = PlTr.getZ3();
	double alpha_t = PlTr.gc1.g1.getAlpha_t();
	double alpha_tw = PlTr.gc1.getAlpha_tw();
	double k = PlTr.getK();
	if (k == 1)
		return true;
	A = (z1 + z2) * cos(alpha_t * M_PI / 180) / cos(alpha_tw) * sin(M_PI / k);

	double mt2 = PlTr.gc1.g2.getMt();		// 1й ряд
	double xt2 = PlTr.gc1.g2.getXt();
	double hta2 = PlTr.gc1.g2.getHta();
	double alpha_t2 = PlTr.gc1.g2.getAlpha_t();
	double alpha_tw2 = PlTr.gc1.getAlpha_tw();
	double y2 = z2 * (cos(alpha_t2 * M_PI / 180) / cos(alpha_tw2) - 1);
	double dy2 = 2 * xt2 - y2;
	
	double mt3 = PlTr.gc2.g1.getMt();		// 2й ряд
	double xt3 = PlTr.gc2.g1.getXt();
	double hta3 = PlTr.gc2.g1.getHta();
	double alpha_t3 = PlTr.gc1.g2.getAlpha_t();
	double alpha_tw3 = PlTr.gc1.getAlpha_tw();
	double y3 = z3 * (cos(alpha_t3 * M_PI / 180) / cos(alpha_tw3) - 1);
	double dy3 = 2 * xt3 - y3;
	
	double B2 = (z2 / 2 + xt2 + hta2 - dy2);
	double B3 = (z3 / 2 + xt3 + hta3 - dy3);
	if (B2 >= B3) 
	{
		return (A > B2);
	}
	else 
	{
		return (A > B3);
	}
}

#define PMAX 6		//количество максимальных проворотов при сборке (Сащ сказал до 6-8)
// 6 Условие (Сборка)
bool assembly(const planetary_transmission& PlTr) 
{
	double U1H = PlTr.getU1h_k();
	int z1 = PlTr.getZ1();
	int k = PlTr.getK();
	int p = 1;
	for (	;p < PMAX; p++)			// цикл по числу p
	{
		double res = U1H * z1 / k * (1 + p * k);
		if ((res - (int)res) == 0)
		{
			return true;
		}
	}
	return false;	// возвращает 0, если не выполнено!!!
}

double maxRw(const planetary_transmission PlTr)
{
	double rw_max;
	switch (PlTr.getType())
	{
	case 1:
		rw_max = PlTr.gc2.getRw2();
		break;
	case 2:
		rw_max = PlTr.gc1.getRw1() + 2 * PlTr.gc1.getRw2();
		if (rw_max < PlTr.gc2.getRw2())
		{
			rw_max = PlTr.gc2.getRw2();
		}
		break;
	case 3:
		rw_max = PlTr.gc1.getRw1();
		if (rw_max < PlTr.gc2.getRw2())
		{
			rw_max = PlTr.gc2.getRw2();
		}
		break;
	case 4:
		rw_max = PlTr.gc1.getRw1() + 2 * PlTr.gc1.getRw2();
		if (rw_max < (PlTr.gc2.getRw2() + 2 * PlTr.gc2.getRw1()))
		{
			rw_max = PlTr.gc2.getRw2() + 2 * PlTr.gc2.getRw1();
		}
		break;
	}
	return rw_max;
}

 // 7 Условие (Минимальность размеров)
const planetary_transmission& minimal(const planetary_transmission* Array, int n)	// желательно n принимать от 1 
{
	double min = maxRw(Array[0]);
	int k = 0;
	for (int i = 0; i < n; i++)
	{
		double temp = maxRw(Array[i]);
		if (min > temp) 
		{
			k = i;
			min = temp;
		}
	}
	return Array[k];
}

void main()
{
	setlocale(LC_ALL, "Russian");

	/*
	cout << inv_to_rad(1) << " - " << rad_to_inv(inv_to_rad(1)) << endl;
	cout << inv_to_rad(0.8) << " - " << rad_to_inv(inv_to_rad(0.8)) << endl;
	cout << inv_to_rad(0.6) << " - " << rad_to_inv(inv_to_rad(0.6)) << endl;
	cout << inv_to_rad(0.4) << " - " << rad_to_inv(inv_to_rad(0.4)) << endl;
	cout << inv_to_rad(0.2) << " - " << rad_to_inv(inv_to_rad(0.2)) << endl;
	cout << inv_to_rad(0) << endl << endl;


	double U1H = 111.20;
	int z1 = 24;
	int k = 3;
	int END = 10;
	for (int p = 0; p < END; p++)
	{
		double res = U1H * z1 / k * (1 + p * k);
		cout << "res is:		" << res << endl;
		cout << "(int)res is:	" << (int)res << endl << endl;

		if ((res - (int)res) == 0)
		{
			cout << "TRUE" << endl;
			p = END;
		}
	}
	*/

	/*
	int type = 2;
	int k = 1;
	int z1 = 20;
	double m1 = 10;
	int z2 = 23;
	int z3 = 40;
	double m3 = 10;
	int z4 = 100;

	gear gear1(1, z1, m1);
	gear gear2(1, z2, m1, 1);
	gear gear3(1, z3, m3);
	gear gear4(-1, z4, m3, -0.5);

	cout << "	Gear1 :" << endl;
	gear1.print();
	cout << endl;
	cout << "	Gear2 :" << endl;
	gear2.print();
	cout << endl;
	cout << "	Gear3 :" << endl;
	gear3.print();
	cout << endl;
	cout << "	Gear4 :" << endl;
	gear4.print();
	cout << endl;

	planetary_transmission PT(type, k, gear1, gear2, gear3, gear4);

	// Проверка выполнения 1 условия
	cout << "Условие выполнения передаточного отношения :" << endl;
	double ERROR = 0.2;
	cout << PT.getU1k_h() << "   " << PT.getUk1_h() << "   " << PT.getU1h_k() << "   " << PT.getUh1_k() << "   " << endl;
	cout << gearratio(PT, -2.87, ERROR) << "   " << gearratio(PT, -0.35, ERROR) << "   " << gearratio(PT, 3.9, ERROR) << "   " << gearratio(PT, 0.258, ERROR) << endl << endl;

	// Проверка выполнения 2 условия
	cout << "Условие отсутствия подрезания :" << endl;
	cout << pruning_zmin(gear1) << " => " << (gear1.getZ() >= pruning_zmin(gear1)) << endl;
	cout << pruning_zmin(gear2) << " => " << (gear2.getZ() >= pruning_zmin(gear2)) << endl;
	cout << pruning_zmin(gear3) << " => " << (gear3.getZ() >= pruning_zmin(gear3)) << endl;
	cout << pruning_zmin(gear4) << " => " << (gear4.getZ() >= pruning_zmin(gear4)) << endl;

	// Проверка выполнения 4 условия
	cout << "Условие соосности :" << endl;
	cout << alignment(PT) << endl << endl;

	// Проверка выполнения 5 условия
	cout << "Условие соседства :" << endl;
	cout << neighbourhood(PT) << endl << endl;

	// Проверка выполнения 6 условия
	cout << "Условие сборки :" << endl;
	cout << assembly(PT) << endl << endl;

	// Проверка выполнения 7 условия
	cout << "Условие минимальности размеров :" << endl;
	cout << maxRw(PT) << endl;
	cout << "rw1 + 2 * rw2 = " << PT.gc1.getRw1() << " + " << "2 * " << PT.gc1.getRw2() << " = " << PT.gc1.getRw1() + 2 * PT.gc1.getRw2() << endl;
	cout << "rw4 = " << PT.gc2.getRw2() << endl << endl;
	*/

	
	// алгоритм программы 
	double modules[20] = { 1.25, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10, 12, 16, 20, 25, 32, 40, 50, 60, 80, 100 };	// модули первого ряда (из ГОСТа)

	int Z1MIN = 20;
	int Z1MAX =	60;
	double X1MIN = 0;
	double X1MAX = 0;
	double dx = 0.005;	// шаг 
	int M1MIN = 0;		 // минимальный индекс в массиве модулей
	int M1MAX = 20;		 // максимальный индекс в массиве модулей
	int Z2MIN = 20;
	int Z2MAX = 100;
	double X2MIN = 0;
	double X2MAX = 0;
	int Z3MIN = 20;
	int Z3MAX = 100;
	double X3MIN = 0;
	double X3MAX = 0;
	int M2MIN = 0;		// минимальный индекс в массиве модулей
	int M2MAX = 20;		 // максимальный индекс в массиве модулей
	int Z4MIN = 50;
	int Z4MAX = 200;
	double X4MIN = 0;
	double X4MAX = 0;

	double ALPHA1 = 20;			// alpha зацепления одинаково
	double ALPHA2 = 20;
	double BETA1 = 0;			// beta зацепления одинаково
	double BETA2 = 0;

	double Ha1 = 1;		//h*a = 1 или 0.8
	double Ha2 = 1;		//h*a = 1 или 0.8
	double Ha3 = 1;		//h*a = 1 или 0.8
	double Ha4 = 1;		//h*a = 1 или 0.8

	double C1 = 0.25;	// стандартный c = 0.25, но можно ввести свой	
	double C2 = 0.25;		
	double C3 = 0.25;		
	double C4 = 0.25;

	double U_theoretic = 2; // теореьтическое передаточное отношение
	double Error = 5;	// погрешность в % передаточного отношения

	int K = 2;			// число сателлитов

	int type = 2;		// тип схемы

	gear gear1;
	gear gear2;
	gear gear3;
	gear gear4;
	gear_connection gc1;
	gear_connection gc2;
	planetary_transmission PlTr;

	//массивы и переменные для сохранения правильных планетарок
	planetary_transmission* Arr;		//масссив из правильных планетарок
	planetary_transmission* temp;		//масссив для сохранения элементов
	int num = 0;						//количество планетарок в массиве
	temp = new planetary_transmission[num + 1];
	Arr = new planetary_transmission[num + 1];

	int l = 0;
	cout << l << endl;

	
	for (int z1 = Z1MIN; z1 <= Z1MAX; z1++)														//цикл для z1 
	{
		l += 1;
		cout << l << endl;

		for (double x1 = X1MIN; x1 <= X1MAX; x1 = x1 + dx)										//цикл для x1 с шагом dx
		{
			for (int tmp1 = M1MIN; tmp1 < M1MAX; tmp1++)										//цикл для индекса в массиве модулей
			{
				double m1 = modules[tmp1];
				for (int z2 = Z2MIN; z2 <= Z2MAX; z2++)											//цикл для z2 
				{
					for (double x2 = X2MIN; x2 <= X2MAX; x2 = x2 + dx)							//цикл для x2 с шагом dx 
					{

						for (int z3 = Z3MIN; z3 <= Z3MAX; z3++)									//цикл для z3 
						{
							for (double x3 = X3MIN; x3 <= X3MAX; x3 = x3 + dx)					//цикл для x3 с шагом dx 
							{
								for (int tmp2 = M2MIN; tmp2 < M2MAX; tmp2++)					//цикл для индекса в массиве модулей
								{
									double m2 = modules[tmp2];			//выбирается модуль из массива модулей

									for (int z4 = Z4MIN; z4 <= Z4MAX; z4++)									//цикл для z4 
									{
										for (double x4 = X4MIN; x4 <= X4MAX; x4 = x4 + dx)					//цикл для x4 с шагом dx 
										{
											if (type == 1)
											{
												gear1.Init(1, z1, m1, x1, ALPHA1, BETA1, Ha1, C1);
												gear2.Init(1, z2, m1, x2, ALPHA1, BETA1, Ha2, C2);
												gear3.Init(-1, z3, m1, x3, ALPHA1, BETA1, Ha3, C3);
												gc1.Init(gear1, gear2);
												gc2.Init(gear2, gear3);
												PlTr.InitPT(type, K, gc1, gc2);
											}
											if (type == 2)
											{
												gear1.Init(1, z1, m1, x1, ALPHA1, BETA1, Ha1, C1);
												gear2.Init(1, z2, m1, x2, ALPHA1, BETA1, Ha2, C2);
												gear3.Init(1, z3, m2, x3, ALPHA2, BETA2, Ha3, C3);
												gear4.Init(-1, z4, m2, x4, ALPHA2, BETA2, Ha4, C4);
												gc1.Init(gear1, gear2);
												gc2.Init(gear3, gear4);
												PlTr.InitPT(type, K, gc1, gc2);
											}
											if (type == 3)
											{
												gear1.Init(-1, z1, m1, x1, ALPHA1, BETA1, Ha1, C1);
												gear2.Init(1, z2, m1, x2, ALPHA1, BETA1, Ha2, C2);
												gear3.Init(1, z3, m2, x3, ALPHA2, BETA2, Ha3, C3);
												gear4.Init(-1, z4, m2, x4, ALPHA2, BETA2, Ha4, C4);
												gc1.Init(gear1, gear2);
												gc2.Init(gear3, gear4);
												PlTr.InitPT(type, K, gc1, gc2);
											}
											if (type == 4)
											{
												gear1.Init(1, z1, m1, x1, ALPHA1, BETA1, Ha1, C1);
												gear2.Init(1, z2, m1, x2, ALPHA1, BETA1, Ha2, C2);
												gear3.Init(1, z3, m2, x3, ALPHA2, BETA2, Ha3, C3);
												gear4.Init(1, z4, m2, x4, ALPHA2, BETA2, Ha4, C4);
												gc1.Init(gear1, gear2);
												gc2.Init(gear3, gear4);
												PlTr.InitPT(type, K, gc1, gc2);
											}

											if (gearratio(PlTr, U_theoretic, Error) == 0)		//проверка 1 условия (передаточное отношение)
											{
												continue;
											}

											if (pruning(PlTr) == 0)								//проверка 2 условия (внешнее колесо)
											{
												continue;
											}

											l += 1;
											cout << l << endl;

											if (in_connection(PlTr) == 0)						//проверка 3 условия (внутреннее зацепление)
											{
												continue;
											}

											if (alignment(PlTr) == 0)							//проверка 4 условия (соосность)
											{
												continue;
											}
											
											if (neighbourhood(PlTr) == 0)						//проверка 5 условия (соседство)
											{
												continue;
											}

											if (assembly(PlTr) == 0)							//проверка 6 условия (сборка)
											{
												continue;
											}

											for (int i = 0; i < num; i++) 
											{
												temp[i] = Arr[i];
											}
											num += 1;
											Arr = new planetary_transmission[num + 1];
											for (int i = 0; i < num; i++)
											{
												Arr[i] = temp[i];
											}
											Arr[num] = PlTr;
											delete[] temp;
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	minimal(Arr, num);	// нахождение минимального планетарного механизма

	delete[] Arr;

	system("PAUSE");
}
