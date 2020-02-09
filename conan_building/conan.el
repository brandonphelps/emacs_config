

(defun conan/build ()
  (let (root_dir (locate-dominating-file "." "conanfile.py"))
    (mkdir (concat root_dir "build"))
    (shell-command "conan install -if build" "*conan_buffer*")
    ))
  
  


(defun tmp ()
  (interactive)
  (let ((root_dir (locate-dominating-file "." "conanfile.py")))
    (mkdir (concat root_dir "build") t)
    (setq tmp_command "conan install -if build .")
    (cd root_dir)
    (with-current-buffer (generate-new-buffer "*command_buffer*")
      (insert (shell-command-to-string "conan install -if build . &"))
      (if (file-exists-p (concat root_dir (file-name-as-directory "build") "conanbuildinfo.txt"))
	  (insert (shell-command-to-string "conan build -if build . &"))))
    )
  (display-buffer-in-side-window "*command_buffer*" 0)
  )

