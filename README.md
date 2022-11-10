# 425-Project
Computational Physics (PHYCS425) project for Fall 2021. The project studies properties of constrained motion 
in 1D and 2D surfaces using RK-4 scheme for integrating equations of motion.

## Project Description
There are other forms of constrained motion, like that of a fluid moving in a pipe. But
our focus in this project is only on the former. These types of constraints fall under the
category of “holonomic constraints”. For each one of these constraints, the system loses
a degree of freedom.

If one wants to investigate such motion analytically, Newton’s laws of motion are not
really helpful. One usually resorts to Lagrangian or Hamiltonian mechanics to get any sort
of insight from these systems. Even then, getting exact solutions is cumbersome, and
when damping is added, it becomes *exponentially* (pun intended) more difficult to obtain. This is where
computer simulations come in handy. They allow us to study the properties of motion
without knowing the most general form of the solution.

## Results
* We first investigated the motion on 2 curves: a parabola $C_1$, and an unknown
curve $C_2$, which we sought to identify. We also found the equilibrium points for both. 
* We then studied the periodic motion in detail, exploring the effect of various parameters like the
dimension of the curves & initial displacement. 
* We then added damping and driving forces to both systems and investigated
the possibility of chaotic motion.
* We then considered a spherical surface. We investigated the initial conditions that lead
various paths – including closed paths and Lissajous-like figures.
* We investigated if chaotic motion is possible on the sphere, to inconvlusive results.
* In addition, we studied the effects of a homogeneous damping force.
* We then experimented on motion on a hemispherical surface.
