import { Request, Response, NextFunction } from 'express';

export default interface IUserController  {
  createUser(req: Request, res: Response, next: NextFunction);
  updateUser(req: Request, res: Response, next: NextFunction);
  getUser(req: Request, res: Response, next: NextFunction);
  getAllUsers(req: Request, res: Response, next: NextFunction)
  deleteUser(req: Request, res: Response, next: NextFunction)
  login(req: Request, res: Response, next: NextFunction);
  loginWithGoogle(req: Request, res: Response, next: NextFunction);

}