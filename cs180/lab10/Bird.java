public class Bird extends Animal {

	public Bird(String status) {
		super(status);
	}

	@Override
	public void makeMove(SiteGrid sg) {
		if (status.equals("ALIVE")) {
			try {
				SiteType t = getSite().getType();
				switch (t) {
				case FEEDING:
					super.makeMove(sg);
					break;
				case NESTING:
					super.makeMove(sg);
					break;
				case WINTERING:
					die();
					break;
				default:
					break;
				}
			} catch (NullPointerException e) {
				super.makeMove(sg);
				return;
			}

		}
	}
}
