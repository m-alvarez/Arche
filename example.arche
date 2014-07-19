{
	producto := (global) {
		impuestos   [1] := 10;
		precio      [2] := 0;
		precioFinal [2] := (self)
			self.precio + self.class.impuestos;
	};
	
	libro := (global) extend new(global.producto) with {
		impuestos [0] := 7;
		paginas   [1] := 0;
	};
	
	video := (global) extend new(global.producto) with {
		impuestos [0] := 19;
		duracion  [1] := 0;
	};

	moby_dick := (global) extend new(global.libro) with {
		paginas := 822;
		precio  := 9;
	};

	el_padrino := (global) extend new(global.video) with {
		duracion := 143;
		precio   := 19;
	};
}
