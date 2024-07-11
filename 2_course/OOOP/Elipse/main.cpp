#include <SFML/Graphics.hpp>
#include <iostream>
#include <cmath>

using namespace std;
using namespace sf;

bool pointInElipse(Vector2f center, float x0, float y0, float a, float b);
float lenVectors(Vector2f coordinateVector);

int main()
{
    Vector2f ellipseCenter, vectorMove, pointPosition, pointPosition2, dott;
    float semiMajorAxis, semiMinorAxis;
    bool pointMove = false;

    cout << "Enter the center coordinates of the ellipse (x y): ";
    cin >> ellipseCenter.x >> ellipseCenter.y;
    cout << "Enter the semi-major axis: ";
    cin >> semiMajorAxis;
    cout << "Enter the semi-minor axis: ";
    cin >> semiMinorAxis;

    RenderWindow window(VideoMode(1000, 1000), "Ellipse");

    CircleShape ellipse(semiMajorAxis);
    ellipse.setPosition(ellipseCenter);
    ellipse.setOrigin(semiMajorAxis, semiMajorAxis);
    ellipse.setScale(1, semiMinorAxis / semiMajorAxis);
    ellipse.setFillColor(Color::Blue);


    CircleShape point(5);
    point.setOrigin(5, 5);
    point.setFillColor(Color::Red);

    while (window.isOpen())
    {
        Event event;
        while (window.pollEvent(event))
        {
            if (event.type == Event::Closed)
                window.close();

            if (event.type == Event::MouseButtonPressed && event.mouseButton.button == Mouse::Left && !pointMove) {
                pointPosition = window.mapPixelToCoords(Mouse::getPosition(window));
                pointPosition2 = pointPosition;
                point.setPosition(pointPosition);
                window.clear();
                window.draw(ellipse);
                window.draw(point);
                window.display();

                cout << "First X: " << pointPosition2.x << " First Y: " << pointPosition2.y << "\n";
                if (pointInElipse(ellipseCenter, pointPosition.x, pointPosition.y, semiMajorAxis, semiMinorAxis)) {
                    cout << "Enter the motion vector (x, y): ";
                    cin >> vectorMove.x >> vectorMove.y;
                    pointMove = true;
                }
                else {
                    cout << "Point doesn`t inside ellipse\n";
                }
            }
        }

        bool pointInside = pointInElipse(ellipseCenter, pointPosition.x, pointPosition.y, semiMajorAxis, semiMinorAxis);

        if (pointMove && pointInside) {
            pointPosition += vectorMove / 10.f;
            point.setPosition(pointPosition);
            dott = Vector2f(pointPosition.x, pointPosition.y);
        }

        else if (pointMove && !pointInside) {
            cout << "Dott x: " << dott.x << " Dott.y: " << dott.y << "\n";
            //Малювання двох прямих
            VertexArray line(Lines, 2);
            line[0].position = pointPosition2;
            line[1].position = dott;
            line[0].color = Color::Yellow;
            line[1].color = Color::Yellow;

            VertexArray line2(Lines, 2);
            line2[0].position = ellipseCenter;
            line2[1].position = dott;
            line2[0].color = Color::Green;
            line2[1].color = Color::Green;


            //Пошук кординат векторів та їх довжина
            Vector2f normalVectorCord = Vector2f(ellipseCenter.x - dott.x, ellipseCenter.y - dott.y);
            float lenNormalVector = lenVectors(normalVectorCord);
            Vector2f lineVectorCord = Vector2f(pointPosition2.x - dott.x, pointPosition2.y - dott.y);
            float lenLineVector = lenVectors(lineVectorCord);

            cout << "normalVectorCord: " << normalVectorCord.x << " " << normalVectorCord.y << "\n";
            cout << "lenNormalVector: " << lenNormalVector << "\n";

            cout << "lineVectorCord: " << lineVectorCord.x << " " << lineVectorCord.y << "\n";
            cout << "lenLineVector: " << lenLineVector << "\n";

            //Скалярний добуток двох векторів
            float scalarProduct = lineVectorCord.x * normalVectorCord.x + normalVectorCord.y * lineVectorCord.y;
            cout << "scalarProduct: " << scalarProduct << "\n";

            //Косинус кута
            float angleCos = scalarProduct / (lenLineVector * lenNormalVector);
            cout << "Angle cos: " << angleCos << "\n";

            float angleDegree = 2 * (180 - acos(angleCos) * (180.0f / 3.14159f));
            cout << "Angle Degree: " << angleDegree << "\n";


            cout << "Degree: " << angleDegree << "\n";


            //Повертаємо початкову точку на 2 * angleDegree
            float rotatedX = (pointPosition2.x - dott.x) * cos(angleDegree * 3.14159265 / 180) - (pointPosition2.y - dott.y) * sin(angleDegree * 3.14159265 / 180) + dott.x;
            float rotatedY = (pointPosition2.x - dott.x) * sin(angleDegree * 3.14159265 / 180) + (pointPosition2.y - dott.y) * cos(angleDegree * 3.14159265 / 180) + dott.y;
            pointPosition2 = Vector2f(rotatedX, rotatedY);

            //Знайдемо новий вектор руху
            Vector2f newVectorCord = Vector2f(pointPosition2.x - dott.x, pointPosition2.y - dott.y);
            float lenNewVector = lenVectors(newVectorCord);
            newVectorCord /= lenNewVector;
            cout << "New vector: " << newVectorCord.x << " " << newVectorCord.y << "\n";

            //Намалюємо ці прямі

            VertexArray line3(Lines, 2);
            line3[0].position = dott;
            line3[1].position = pointPosition2;
            line3[0].color = Color::White;
            line3[1].color = Color::White;

            window.draw(line);
            window.draw(line2);
            window.draw(line3);
            window.display();

            vectorMove = Vector2f(newVectorCord.x, newVectorCord.y);
            pointPosition2 = Vector2f(dott.x, dott.y);

            cout << "Vector move: " << vectorMove.x << " " << vectorMove.y << "\n";

            pointPosition = dott;
            pointPosition += vectorMove;
            point.setPosition(pointPosition);

            int a = 0;
            do {
                cin >> a;
            } while (a != 1);

        }
        window.clear();
        window.draw(ellipse);
        window.draw(point);
        window.display();
    }
    return 0;
}


bool pointInElipse(Vector2f center, float x0, float y0, float a, float b) {
    float num1 = ((center.x - x0) / a);
    float num2 = ((center.y - y0) / b);
    return num1 * num1 + num2 * num2 < 1;
}

float lenVectors(Vector2f coordinateVector) {
    return sqrt(coordinateVector.x * coordinateVector.x + coordinateVector.y * coordinateVector.y);
}
