import { Request, Response, NextFunction } from 'express';

export default interface IPathController  {
  getAllPaths(req: Request, res: Response, next: NextFunction)
  
}