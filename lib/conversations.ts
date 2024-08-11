
type Token = {
	id: string;
	?message: Message;
	parent: string; // parent is now a string ID
	//children?: string[]; // array of string IDs
}

type conversation = {
	title: string;
	create_time: number;
	update_time: number;
	mapping: Mapping;
	current_node: string;
}

type jsonData = conversation list;

type Mapping = {
	[key: string]: Token;
}

type Token = {
	id: string;
	?message: Message;
	parent: string; // parent is now a string ID
	//children?: string[]; // array of string IDs
}

type ResultMessage {
	author: Author;
	text: string;
}

type ResultMessages = ResultMessage[];

type Message {
	id: string;
	author: Author;
	create_time: number;
	update_time?: number;
	content: Content;
	status: string;
	end_turn: boolean;
	weight: number;
	metadata: Metadata;
	recipient: string;
}

type Metadata {
	id?: string;
	name?: string;
	mimeType?: string;
	timestamp_?: string;
	message_type?: string;
	attachments?: Attachment[];
	is_user_system_message?: boolean;
}

type Attachment {
	id: string;
	name: string;
	mimeType: string;
}

type Content {
	content_type: string;
	parts: string[];
}

type Author {
	role?: string;
	name?: string;
	metadata?: Metadata;
}
