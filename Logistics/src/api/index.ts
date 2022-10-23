import { Router } from 'express';
import truck from './routes/truckRoute';

export default () => {
	const app = Router();

	truck(app);
	
	return app
}