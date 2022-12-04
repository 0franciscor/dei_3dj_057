import { Router } from 'express';
import truck from './routes/truckRoute';
import path from './routes/pathRoute';
import packaging from './routes/packagingRoute';
import trip from './routes/tripRoute';

export default () => {
	const app = Router();

	truck(app);
	
	path(app);
	packaging(app);
	trip(app);
	
	return app
}
