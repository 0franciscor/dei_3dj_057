import { NextFunction, Request, Response } from 'express';

import { Inject, Service } from 'typedi';

import config from '../../config';


import { Result } from '../core/logic/Result';

import { IUserDTO } from '../dto/IUserDTO';
import IUserService from '../services/IServices/IUserService';
import IUserController from './IControllers/IUserController';
const {OAuth2Client} = require('google-auth-library');
const client = new OAuth2Client(config.googleClientId);
const jwt = require('jsonwebtoken');

@Service()
export default class UserController implements IUserController{
    constructor(
        @Inject(config.services.user.name)
        private userService: IUserService
    ) {}

    public async loginWithGoogle(req: Request, res: Response, next: NextFunction) {
        try {
            const token = req.body.credentials;
            const ticket = await client.verifyIdToken({
                idToken: token,
                audience: config.googleClientId
            });
            
            const payload = ticket.getPayload();

            const userExists = await this.userService.getUserByEmail(payload['email']);
            if(userExists.isFailure){
                
                const userDTO = {
                    firstName: payload['given_name'],
                    lastName: payload['family_name'],
                    email: payload['email'],
                    password: payload['sub'],
                    role: "user"
                } as IUserDTO;
                
                const userOrError = await this.userService.createUser(userDTO);
                
                if(userOrError.isFailure){
                    res.status(401);
                    return res.send("Invalid credentials");
                }
            }
            
            const userDTO = userExists.getValue();
            const cookie = jwt.sign({id: userDTO.id.toString(),role:userDTO.role.toString()}, config.jwtSecret, {expiresIn: 86400});
            res.status(200);
            res.cookie('jwt', cookie, {maxAge: 86400000});

            return res.json({token: cookie});;
        } catch (error) {
            next(error);
        }
    }



    public async login(req: Request, res: Response, next: NextFunction) {
    
        try {
            const userOrError = await this.userService.login(req.body as IUserDTO);
            if(userOrError.isFailure){
                res.status(401);
                return res.send("Invalid credentials");
            }
            const userDTO = userOrError.getValue();
            const token = jwt.sign({id: userDTO.id.toString(),role:userDTO.role.toString()}, config.jwtSecret, {expiresIn: 86400});
            res.status(200);
            res.cookie('jwt', token, {maxAge: 86400000});
            
            return res.json({token: token});
        } catch (error) {
            next(error);
        }
    }

  

    public async getUser(req: Request, res: Response, next: NextFunction){
        try{
            const user = await this.userService.getUserByEmail(req.body.email);
            if(user.isFailure){
                res.status(404);
                return res.send("User not found");
            }

            res.status(200);
            return res.json(user.getValue());

        }catch(e){
            next(e);
        }
    }

    public async getAllUsers(req:Request,res:Response,next:NextFunction){
        try {
            const users = await this.userService.getAllUsers();
            res.status(200);
            return res.json(users.getValue());
        } catch (error) {
            next(error);
        }
    }

    public async createUser(req:Request,res:Response,next:NextFunction){
        try {
            const userOrError = await this.userService.createUser(req.body as IUserDTO);
            if(userOrError.isFailure){
                res.status(409);
                return res.send("user already exists");
            }

            const userDTO = userOrError.getValue();
            
            res.status(201);
            return res.json(userDTO);

        } catch (error) {
            next(error);
        }
    }

    public async updateUser(req:Request,res:Response,next:NextFunction){
        try {
            const userOrError = await this.userService.updateUser(req.body as IUserDTO) as Result<IUserDTO>;
            if(userOrError.isFailure){
                res.status(404);
                return res.send("user not found");
            }

            const userDTO = userOrError.getValue();
            res.status(201);
            return res.json(userDTO);
        } catch (error) {
            next(error)
        }
    }

    public async deleteUser(req:Request,res:Response,next:NextFunction){
        try {
            const userResult = await this.userService.deleteUser(req.body.email);
            if(userResult.isFailure){
                res.status(404);
                return res.send("user not found");
            }
            res.status(200)
            return res.json(userResult.getValue());                
        } catch (error) {
            next(error)
        }
    }
}


