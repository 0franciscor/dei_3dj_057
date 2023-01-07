import { Request, Response, NextFunction } from 'express';

export default interface IWarehouseController  {
  
  //#################### Domain Related, Warehouses ####################

  getAllWarehouse(req: Request, res: Response, next: NextFunction);

  getWarehouse(req: Request, res: Response, next: NextFunction);

  createWarehouse(req: Request, res: Response, next: NextFunction);

  editWarehouse(req: Request, res: Response, next: NextFunction);

  activateWarehouse(req: Request, res: Response, next: NextFunction);

  deactivateWarehouse(req: Request, res: Response, next: NextFunction);

  deleteWarehouse(req: Request, res: Response, next: NextFunction);


  //#################### Domain Related, Cities ####################
  
  getAllCities(req: Request, res: Response, next: NextFunction);


  //#################### Prolog Methods ####################

  createWarehouseProlog(req: Request, res: Response, next: NextFunction);

  updateWarehouseProlog(req: Request, res: Response, next: NextFunction);

  deleteWarehouseProlog(req: Request, res: Response, next: NextFunction);

  createCityProlog(req: Request, res: Response, next: NextFunction);
  
}