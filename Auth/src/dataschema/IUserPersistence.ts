export interface IUserPersistence {
	_id: string;
	userId:string
	firstName: string;
	lastName: string;
	email: string;
	password: string;
	salt: string;
	role: string
  }