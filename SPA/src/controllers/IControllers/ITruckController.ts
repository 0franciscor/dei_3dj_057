import { Request, Response, NextFunction } from 'express';

export default interface ITruckController  {
  
  createTruck(req: Request, res: Response, next: NextFunction)
  getAllTruck(req: Request, res: Response, next: NextFunction)
  getTruck(req: Request, res: Response, next: NextFunction)
  editTruck(req: Request, res: Response, next: NextFunction)
  
}