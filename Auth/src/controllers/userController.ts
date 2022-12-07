import { Request, Response, NextFunction } from 'express';

import { Container} from 'typedi';
import { Inject, Service } from 'typedi';

import config from '../../config';

import IUserRepo from '../services/IRepos/IUserRepo';

import { Result } from '../core/logic/Result';

import { UserMap } from "../mappers/UserMap";
import { IUserDTO } from '../dto/IUserDTO';
import { ServerCapabilities } from 'mongodb';
import IUserController from './IControllers/IUserController';
import IUserService from '../services/IServices/IUserService';
import { ParamsDictionary } from 'express-serve-static-core';
import { ParsedQs } from 'qs';
const jwt = require('jsonwebtoken');

@Service()
export default class UserController implements IUserController{
    constructor(
        @Inject(config.services.user.name)
        private userService: IUserService
    ) {}



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
            res.cookie('jwt', token, {httpOnly: true,maxAge: 86400000});
            return res.send("Login successful");
        } catch (error) {
            next(error);
        }


        

    }

    public async getUser(req: Request, res: Response, next: NextFunction){
        try{
            const user = await this.userService.getUser(req.body.userId);
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
            console.log(userOrError)
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
            const userResult = await this.userService.deleteUser(req.body.userId);
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


