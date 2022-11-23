import { Router } from 'express';
import pathRoute from './routes/pathRoute';
import roleRoute from './routes/roleRoute';
import user from './routes/userRoute';
import warehouseRoute from './routes/warehouseRoute';
import deliveryRoute from './routes/deliveryRoute';

export default () => {
	const app = Router();

	pathRoute(app);	
	user(app);
	warehouseRoute(app);
	deliveryRoute(app);
	roleRoute(app);
	
    return app;
}