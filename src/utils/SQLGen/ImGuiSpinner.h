namespace ImGui {

	bool Spinner(const char* label, float radius, int thickness, const ImU32& color);

	void LoadingIndicatorCircle(const char* label, const float indicator_radius,
		const ImVec4& main_color, const ImVec4& backdrop_color,
		const int circle_count, const float speed);
}