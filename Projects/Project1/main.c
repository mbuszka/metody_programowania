#include <stdlib.h>
#include <stdio.h>
#include <math.h>

    // kompilować z flagą -lm           
    // układ współrzędnych sześcianu:
    /*  ^ y
        |
        |
        |
        +-------> x
       /
      /
   z /
    \/ 
    
    */s
    
    
typedef double Point[3];
typedef Point Vec;
typedef double Mat[3][3];

#define PI 3.141592
#define copy_pt(src, dest) do{dest[0] = src[0]; dest[1] = src[1]; dest[2] = src[2];} while (0)

#define scale(alpha, p) do{p[0] *= alpha; p[1] *= alpha; p[2] *= alpha;}while(0) 

Mat rotXp = {{1,0,0},{0,0,-1},{0,1,0}};
Mat rotXm = {{1,0,0},{0,0,1},{0,-1,0}};
Mat rotYp = {{0,0,1},{0,1,0},{-1,0,0}};
Mat rotYm = {{0,0,-1},{0,1,0},{1,0,0}};
Mat rotZp = {{0,-1,0},{1,0,0},{0,0,1}};
Mat rotZm = {{0,1,0},{-1,0,0},{0,0,1}};
Mat identity = {{1,0,0},{0,1,0},{0,0,1}};

Vec cube_translation = {0,0,0};
Mat cube_rotation = {{1,0,0},{0,1,0},{0,0,1}};
double obs_dist = 1000;
double pic_len = 500.0;
double cube_len = 200.0;

void rotate(const Mat m, const Vec v, Vec rv) {
    rv[0] = rv[1] = rv[2] = 0;
    for (int i=0; i<3; i++) {
        rv[0] += v[i] * m[0][i];
        rv[1] += v[i] * m[1][i];
        rv[2] += v[i] * m[2][i];
    }
}

void translate(Point p, const Vec v) {
    for (int i=0; i<3; i++) {
        p[i] += v[i];
    }
}

Vec moves[7] = {{0,-1,0},{0,0,1},{0,1,0},{1,0,0},
                {0,-1,0},{0,0,-1},{0,1,0}};

Mat rotations[8];

void copy_mat(const Mat src, Mat dest) {
    for (int i=0; i<3; i++) {
        for (int j=0; j<3; j++) {
            dest[i][j] = src[i][j];
        }
    }
}

void mult_mat(const Mat a, const Mat b, Mat res) {
    for (int i=0; i<3; i++)
        for (int j=0; j<3; j++){
            res[i][j] = 0;
            for (int k=0; k<3; k++) {
                res[i][j] += a[i][k] * b[k][j];
            }
        }
}

void printPoint(const Point p) {
    printf("(%f,%f,%f)\n",p[0],p[1],p[2]);
}

void printMat(const Mat m) {
    for (int i=0; i<3; i++) {
        printf("%f,%f,%f\n",m[i][0],m[i][1],m[i][2]);
    }
    putchar('\n');
}

void project(const Point p) {
    double pr[2];
    Point a;
    rotate(cube_rotation, p, a);
    translate(a, cube_translation);
    pr[0] = a[0]*obs_dist/abs(a[2]);
    pr[1] = a[1]*obs_dist/abs(a[2]);
    printf("%f %f ",pr[0]+pic_len/2,pr[1]+pic_len/2);
}

void hilbert(int deg, double alpha, Point p, const Mat rotation) {
    if (deg == 0) {
        return;
    }
    Mat m; Vec v;
    for (int i=0; i<8; i++) {
        mult_mat(rotation, rotations[i],m);
        hilbert(deg-1, alpha, p, m);
        if (i!=7) {
            rotate(rotation,moves[i],v);
            scale(alpha,v);
            translate(p, v);
            project(p); printf("lineto\n");
        }
    }
}

void calculate_rotations() {
    mult_mat(rotXm,rotYp,rotations[0]);
    mult_mat(rotZp,rotYm,rotations[1]);
    mult_mat(rotZp,rotYm,rotations[2]);
    mult_mat(rotXm,rotXm,rotations[3]);
    mult_mat(rotXm,rotXm,rotations[4]);
    mult_mat(rotZm,rotYp,rotations[5]);
    mult_mat(rotZm,rotYp,rotations[6]);
    mult_mat(rotXm,rotYm,rotations[7]);
}

int main(int argc, char *args[]) {
    calculate_rotations();
    int deg =2;
    double x,y,z,phi,psi;
    if (argc != 10 || 
        (sscanf(args[1],"%d",&deg)+
        sscanf(args[2],"%lf",&pic_len)+
        sscanf(args[3],"%lf",&cube_len)+
        sscanf(args[4],"%lf",&obs_dist)+
        sscanf(args[5],"%lf",&x)+
        sscanf(args[6],"%lf",&y)+
        sscanf(args[7],"%lf",&z)+
        sscanf(args[8],"%lf",&phi)+
        sscanf(args[9],"%lf",&psi)) 
        != 9) {
        fprintf(stderr,"usage :%s n s u d x y z phi psi\n", args[0]);
        return 0;
    }
    double alpha = cube_len/(pow(2.0,deg) - 1);
    Point p = {-cube_len/2,cube_len/2,-cube_len/2};
    phi *= PI/180.0; psi *= PI/180.0;
    
    Mat phi_mat = {{1,0,0},{0,cos(phi),-sin(phi)},{0,sin(phi),cos(phi)}};
    Mat psi_mat = {{cos(psi),0,sin(psi)},{0,1,0},{-sin(psi),0,cos(psi)}};
    mult_mat(phi_mat,psi_mat,cube_rotation);
    cube_translation[0] = x;
    cube_translation[1] = y;
    cube_translation[2] = -obs_dist - z;
    
    printf("%%!PS-Adobe-2.0 EPSF-2.0\n%%%%BoundingBox: %f %f %f %f\nnewpath\n",0.0,0.0,pic_len,pic_len);
    project(p); printf("moveto\n");
    hilbert(deg,alpha,p,identity);
    printf(".4 setlinewidth\nstroke\nshowpage\n%%%%Trailer\n%%EOF\n");
    return 0;
}
