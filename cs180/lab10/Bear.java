public class Bear extends Animal {

	public Bear(String status) {
		super(status);
	}

	@Override
	public void makeMove(SiteGrid sg) {
		if (status.equals("ALIVE")) {
			try {
				switch (getSite().getType()) {
				case FEEDING:
					super.makeMove(sg);
					break;
				case NESTING:
					super.makeMove(sg);
					break;
				case WINTERING:
					if (Math.random() <= 0.3)
						die();
					super.makeMove(sg);
					break;
				default:
					break;

				}
			} catch (NullPointerException e) {
				super.makeMove(sg);
			}
		}
	}
}
