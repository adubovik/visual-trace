#include <GL/glut.h>
#include <stdio.h>

const int fps = 30;
const int refreshMillis = 1000/fps; // Refresh period in milliseconds

/* Global state */
GLfloat clippingAreaX = 0.0;
GLfloat clippingAreaY = 0.0;
GLfloat clippingAreaExtend = 1.0;

bool leftButton = false;
bool rightButton = false;

int mousePosX = 0;
int mousePosY = 0;

GLfloat viewPortLeft,viewPortRight;
GLfloat viewPortTop,viewPortBottom;

/* Global state */

GLfloat viewPortToLocalX(int x) {
  int width  = glutGet(GLUT_WINDOW_WIDTH);
  GLfloat xLocal = (GLfloat)x/(GLfloat)width;
  xLocal = viewPortLeft + xLocal * (viewPortRight - viewPortLeft);
  return xLocal;
}

GLfloat viewPortToLocalY(int y) {
  int height = glutGet(GLUT_WINDOW_HEIGHT);
  GLfloat yLocal = (GLfloat)y/(GLfloat)height;
  yLocal = viewPortBottom + yLocal * (viewPortTop - viewPortBottom);
  return yLocal;
}

void initGL() {
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
}

void display() {
  glClear(GL_COLOR_BUFFER_BIT);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glBegin(GL_QUADS);
    glColor3f(1.0f, 0.0f, 0.0f);
    glVertex2f(-0.5f, -0.5f);
    glVertex2f( 0.5f, -0.5f);
    glVertex2f( 0.5f,  0.5f);
    glVertex2f(-0.5f,  0.5f);
  glEnd();

  glutSwapBuffers();
}

GLfloat calcAspect(GLsizei width, GLsizei height) {
  GLfloat ret = 1.0;
  if (height != 0) {
    ret = (GLfloat)width / (GLfloat)height;
  }
  return ret;
}

void reshape(GLsizei width, GLsizei height) {
  GLfloat aspect = calcAspect(width,height);

  glViewport(0,0,width,height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  GLfloat x = clippingAreaX;
  GLfloat y = clippingAreaY;
  GLfloat ex = clippingAreaExtend;

  if (aspect >= 1.0) {
    viewPortLeft   = x-ex*aspect;
    viewPortRight  = x+ex*aspect;
    viewPortBottom = y-ex;
    viewPortTop    = y+ex;
  } else {
    viewPortLeft   = x-ex;
    viewPortRight  = x+ex;
    viewPortBottom = y-ex/aspect;
    viewPortTop    = y+ex/aspect;
  }
  gluOrtho2D(viewPortLeft,viewPortRight,viewPortBottom,viewPortTop);
}

void Timer(int value) {
  glutPostRedisplay();
  glutTimerFunc(refreshMillis, Timer, 0);
}

void idle() {
  glutPostRedisplay();
}

void keyboard(unsigned char key, int x, int y) {
  switch(key) {
    case 27: //ESC
      exit(0);
      break;
  }
}

void reshareWindow() {
  int width = glutGet(GLUT_WINDOW_WIDTH);
  int height = glutGet(GLUT_WINDOW_HEIGHT);
  glutReshapeWindow(width,height);
}

void mouse(int button, int state, int x, int y) {
  // Mouse wheel up/down

  if (button == 3) {
    if (state == GLUT_DOWN) {
      clippingAreaExtend /= 1.1;
      reshareWindow();
    }
  }

  if (button == 4) {
    if (state == GLUT_DOWN) {
      clippingAreaExtend *= 1.1;
      reshareWindow();
    }
  }

  leftButton  = (button ==  GLUT_LEFT_BUTTON) && (state == GLUT_DOWN);
  rightButton = (button == GLUT_RIGHT_BUTTON) && (state == GLUT_DOWN);

  mousePosX = x;
  mousePosY = y;

  glutPostRedisplay();
}

void motion(int x, int y) {
  if (rightButton) {
    GLfloat dx = viewPortToLocalX(x) - viewPortToLocalX(mousePosX);
    GLfloat dy = viewPortToLocalY(y) - viewPortToLocalY(mousePosY);

    clippingAreaX -= dx;
    clippingAreaY += dy;
    reshareWindow();
  }

  mousePosX = x;
  mousePosY = y;
}

int main(int argc, char** argv) {
  glutInit(&argc, argv);

  glutInitDisplayMode(GLUT_DOUBLE);

  glutInitWindowSize(500, 500);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("GLUT app");

  glutReshapeFunc(reshape);
  glutDisplayFunc(display);
  glutTimerFunc(0,Timer,0);
  //glutIdleFunc(idle);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse);
  glutMotionFunc(motion);

  initGL();
  glutMainLoop();

  return 0;
}
