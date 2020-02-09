(defun conan/build ()
  (interactive)
  (let ((root_dir (locate-dominating-file "." "conanfile.py")))
    (mkdir (concat root_dir "build") t)
    (setq tmp_command "conan install -if build .")
    ; (complication-start (concat "cd " root_dir "&& conan install -if build ."))
    (cd root_dir)
    
    (compile "conan install -if build . && conan build -if build -bf build . ")
    )
  )

(provide 'conan)
