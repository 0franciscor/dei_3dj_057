import { Request, Response, NextFunction } from 'express';

export default interface IDeliveryController  {
  getAllDeliveries(req: Request, res: Response, next: NextFunction);
  getDelivery(req: Request, res: Response, next: NextFunction);
  createDelivery(req: Request, res: Response, next: NextFunction);
  updateDelivery(req: Request, res: Response, next: NextFunction);
  deleteDelivery(req: Request, res: Response, next: NextFunction);

  createDeliveryProlog(req: Request, res: Response, next: NextFunction);
  updateDeliveryProlog(req: Request, res: Response, next: NextFunction);
  deleteDeliveryProlog(req: Request, res: Response, next: NextFunction);
  
  getDeliveryDestination(req: Request, res: Response, next: NextFunction);
}