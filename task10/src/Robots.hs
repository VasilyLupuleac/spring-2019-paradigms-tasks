module Robots where

-- Во второй части этого домашнего задания вам предстоит промоделировать битвы роботов
-- Цель этой части показать, как моделировать концепции из объектно-ориентированного
-- программирования в функциональном стиле

-- Про робота можно думать, что это просто тройка из имени, уровня атаки и уровня жизни
-- Разумно думать, что уровни жизни и атаки -- это неотрицательные целые числа

-- это просто псевдонимы для типов(красоты ради)
type Name = String
type Attack = Int
type Health = Int
type Robot = (Name, Attack, Health)

-- Напишем конструктор для робота
robot :: Name -> Attack -> Health -> Robot
robot name attack hp  = (name, attack, hp)

-- У объектов есть геттеры или аксессоры -- функции, которые
-- позволяют нам получить доступ к состоянию объекта
-- Напишем их и мы

getName :: Robot -> Name
getName (myName, _, _) = myName

getAttack :: Robot -> Attack
getAttack (_, myAttack, _) = myAttack

getHealth :: Robot -> Health
getHealth (_, _, myHealth) = myHealth

-- Шаг 1
-- Аналогичным образом напишите сеттеры, функции, которые устанавливают
-- состояние робота

setName :: Name -> Robot -> Robot
setName myName (_, myAttack, myHealth) = (myName, myAttack, myHealth)

setAttack :: Attack -> Robot -> Robot
setAttack myAttack (myName, _, myHealth) = (myName, myAttack, myHealth)

setHealth :: Health -> Robot -> Robot
setHealth myHealth (myName, myAttack, _) = (myName, myAttack, myHealth)

-- Шаг 2.
-- Напишите функцию, которая ведет себя как __str__
-- То есть возвращает строковое представление о роботе в виде:
-- > marvin = robot "Marvin" 100 500
-- > printRobot marvin
-- > "Marvin, attack: 100, health: 500"

printRobot :: Robot -> String
printRobot (myName, myAttack, myHealth) = myName ++ ", attack: " ++ show myAttack ++ ", health: " ++ show myHealth

-- Давайте теперь научим роботов драться друг с другом
-- Напишем функцию damage которая причиняет роботу урон
damage :: Robot -> Int -> Robot
damage victim amount = let
        health = getHealth victim
        newHealth = health - amount
    in setHealth newHealth victim

-- Шаг 3.
-- Вам понадобится вспомогательная функция isAlive, которая бы проверяла, жив робот или не очень
-- Робот считается живым, если его уровень здоровья строго больше нуля.
isAlive :: Robot -> Bool
isAlive myRobot = ((getHealth myRobot) > 0)

-- Затем, используя функцию damage, напишите функцию, которая моделирует один раунд схватки между
-- двумя роботами
-- Раунд просиходит следующим образом:
-- 1. Атакующий наносит защищаемуся урон равный величине атаки атакующего
-- 2. В качестве результата эта функция возвращает защищающегося робота,
-- но уже с измененным уровнем здоровья
-- Обратите внимание, что неживой робот не может атаковать. В этом случае нужно просто
-- вернуть второго робота, как будто ничего и не было
fight :: Robot -> Robot -> Robot
fight attacker defender | isAlive attacker = damage defender (getAttack attacker)
                        | otherwise = defender

-- Наконец, напишите функцию, которая бы моделировала три раунда схватки между
-- двумя роботами и возвращала бы победителя. Схватка происходит следующим образом:
-- 1. Атакующий робот ударяет защищающегося, как в предыдущей функции
-- 2. Защищающийся робот приходит в себя и ударяет атакующего.
-- 3. Атакующий робот приходит в себя и еще раз ударяет защищающегося
-- В качестве результата эта функция должна вернуть робота, который вышел победителем
-- Победитель определеяется как робот, у которого уровень здоровья строго больше, чем у сопереника
-- Если же так вышло, что после трех раундов у обоих роботов одинаковый уровень жизни, то
-- победителем считается тот, кто ударял первым(то есть атакующий робот)
threeRoundFight :: Robot -> Robot -> Robot
threeRoundFight attacker defender = let
        second_round_defender = fight attacker defender
        third_round_attacker = fight second_round_defender attacker
        third_round_defender = fight third_round_attacker second_round_defender
    in if getHealth third_round_attacker >= getHealth third_round_defender
        then third_round_attacker
        else third_round_defender

-- Шаг 4.
-- Создайте список из трех роботов(Абсолютно любых, но лучше живых, мы собираемся их побить)
roboter :: [Robot]
roboter = [robot "cout << kek" 10 30, robot "cout << lol" 30 10, robot "cout << mem" 20 20]

-- Затем создайте четвертого
neueRobot :: Robot
neueRobot = robot "Segmentation fault" 100 100

-- Используя частичное применение напишите функцию, которая бы принимала на вход робота
-- и атаковала бы его роботом neueRobot
neueRobotAttak :: Robot -> Robot
neueRobotAttak = fight neueRobot

-- Наконец, используя filter определите, кто из роботов, которых вы положили в список roboter,
-- выживет, если neueRobot сразится с ним в одном раунде.
survivors :: [Robot]
survivors = filter (isAlive . neueRobotAttak) roboter
