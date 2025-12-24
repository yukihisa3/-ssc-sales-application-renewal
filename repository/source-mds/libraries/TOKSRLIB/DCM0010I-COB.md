# DCM0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/DCM0010I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　      *
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　オンライン      *
*    モジュール名　　　　：　実行履歴リスト発行指示            *
*    作成日／作成者　　　：　2022/05/16  NAV INOUE             *
*    処理概要　　　　　　：　画面より、リスト出力条件を入力    *
*                            し、チェック後パラメタに渡す。　　*
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　    *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            DCM0010I.
*                  流用:SSY8864I.TOKSLIBS
 AUTHOR.                NAV .
 DATE-WRITTEN.          2022/05/16.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 実行履歴ファイル >>--*
     SELECT   JIKRERF   ASSIGN         DA-01-VI-JIKRERL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JIK-F01   JIK-F02
                                       JIK-F03   JIK-F04
                                       JIK-F05   JIK-F06
                                       JIK-F07
                        STATUS         JIKRERF-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FCM0010I  OF        XMDLIB.
*----<< 実行履歴ファイル >>--*
 FD  JIKRERF            LABEL     RECORD   IS   STANDARD.
     COPY     JIKRERF   OF        XFDLIB
              JOINING   JIK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JIKRERF-ST        PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WK-SOKCD           PIC  X(02)  VALUE  SPACE.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"起動日（開始）を入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"起動日（終了）を入力して下さい。".
     03  MSG04               PIC  N(20)  VALUE
              NC"　　　　　　　　　　　　".
     03  MSG05               PIC  N(20)  VALUE
              NC"実行履歴ファイルに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"出力区分に誤りがあります。".
     03  MSG07               PIC  N(20)  VALUE
              NC"開始が終了を超えています。".
     03  MSG08               PIC  N(20)  VALUE
              NC"起動日（開始）を正しく入力してください。".
     03  MSG09               PIC  N(20)  VALUE
              NC"起動日（終了）を正しく入力してください。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       9.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-OUT-KDATEF        PIC   9(08).
 01  PARA-OUT-KDATET        PIC   9(08).
 01  PARA-OUT-KTIMEF        PIC   9(06).
 01  PARA-OUT-KTIMET        PIC   9(06).
 01  PARA-OUT-WSIDF         PIC   X(08).
 01  PARA-OUT-WSIDT         PIC   X(08).
 01  PARA-OUT-TANF          PIC   X(02).
 01  PARA-OUT-TANT          PIC   X(02).
 01  PARA-OUT-SYORIF        PIC   9(10).
 01  PARA-OUT-SYORIT        PIC   9(10).
 01  PARA-OUT-OUTKBN        PIC   X(01).
****************************************************************
 PROCEDURE              DIVISION USING PARA-OUT-KDATEF
                                       PARA-OUT-KDATET
                                       PARA-OUT-KTIMEF
                                       PARA-OUT-KTIMET
                                       PARA-OUT-WSIDF
                                       PARA-OUT-WSIDT
                                       PARA-OUT-TANF
                                       PARA-OUT-TANT
                                       PARA-OUT-SYORIF
                                       PARA-OUT-SYORIT
                                       PARA-OUT-OUTKBN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### DCM0010I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 実行履歴ファイル >>--*
 JIKRERF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JIKRERF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### DCM0010I JIKRERF ERROR " JIKRERF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** DCM0010I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     JIKRERF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< 条件 ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    JIKRERF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** DCM0010I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FCM0010I.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "DCM0010I"      TO   PGID.
     MOVE    "FCM0010I"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FCM0010I"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      条件　入力　　　                            *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      条件　　ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
 220-GRP01-CHECK-01.
*    起動日（開始）入力チェック
     IF     ( KDATEF    NOT  NUMERIC ) OR
            ( KDATEF    =    ZERO    )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF KDATEF
              MOVE     "R"        TO   EDIT-OPTION OF KDATEF
              GO   TO   220-GRP01-CHECK-EXIT
     END-IF.
 220-GRP01-CHECK-02.
*    起動日（開始）値チェック
     IF  KDATEF    NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      KDATEF    TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              IF        ERR-FLG   =    ZERO
                        MOVE      8    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF KDATEF
              MOVE     "R"        TO   EDIT-OPTION OF KDATEF
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
 220-GRP01-CHECK-03.
*    起動日（終了）入力チェック
     IF     ( KDATET    NOT  NUMERIC ) OR
            ( KDATET    =    ZERO    )
              MOVE      3         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF KDATET
              MOVE     "R"        TO   EDIT-OPTION OF KDATET
              GO   TO   220-GRP01-CHECK-EXIT
     END-IF.
 220-GRP01-CHECK-04.
*    起動日（終了）値チェック
     IF  KDATET    NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      KDATET    TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF KDATET
              MOVE     "R"        TO   EDIT-OPTION OF KDATET
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
 220-GRP01-CHECK-05.
*　　起動日　範囲チェック
     IF       KDATEF      >  KDATET
              IF    ERR-FLG   =    ZERO
                    MOVE    7      TO   ERR-FLG
                    MOVE   "R"     TO   EDIT-OPTION  OF  KDATEF
                    MOVE   "R"     TO   EDIT-OPTION  OF  KDATET
                    MOVE   "C"     TO   EDIT-CURSOR  OF  KDATEF
                    GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*    ELSE
*             MOVE  "M"      TO   EDIT-OPTION  OF  KDATEF
*             MOVE  "M"      TO   EDIT-OPTION  OF  KDATET
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  KDATEF
     END-IF.
*
 220-GRP01-CHECK-06.
*    起動時刻（開始）
     IF     ( KTIMEF    NOT  NUMERIC ) OR
            ( KTIMEF    =    ZERO    )
              MOVE      ZERO      TO   KTIMEF
     END-IF.
 220-GRP01-CHECK-07.
*    起動時刻（終了）
     IF     ( KTIMET    NOT  NUMERIC ) OR
            ( KTIMET    =    ZERO    )
              MOVE      999999    TO   KTIMET
     END-IF.
  220-GRP01-CHECK-08.
*　　起動時刻 範囲チェック
     IF       KTIMEF      >  KTIMET
              IF    ERR-FLG   =    ZERO
                    MOVE    7      TO   ERR-FLG
                    MOVE   "R"     TO   EDIT-OPTION  OF  KTIMEF
                    MOVE   "R"     TO   EDIT-OPTION  OF  KTIMET
                    MOVE   "C"     TO   EDIT-CURSOR  OF  KTIMEF
                    GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*    ELSE
*             MOVE  "M"      TO   EDIT-OPTION  OF  KTIMEF
*             MOVE  "M"      TO   EDIT-OPTION  OF  KTIMET
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  KTIMEF
     END-IF.
*
  220-GRP01-CHECK-09.
*    ＷＳ番号（開始）
*    ＷＳ番号（終了）
     IF       WSIDT     =    SPACE
              MOVE      "99999999" TO   WSIDT
     END-IF.
 220-GRP01-CHECK-10.
*　　ＷＳ番号 範囲チェック
     IF       WSIDF       >  WSIDT
              IF    ERR-FLG   =    ZERO
                    MOVE    7      TO   ERR-FLG
                    MOVE   "R"     TO   EDIT-OPTION  OF  WSIDF
                    MOVE   "R"     TO   EDIT-OPTION  OF  WSIDT
                    MOVE   "C"     TO   EDIT-CURSOR  OF  WSIDF
                    GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*    ELSE
*             MOVE  "M"      TO   EDIT-OPTION  OF  WSIDF
*             MOVE  "M"      TO   EDIT-OPTION  OF  WSIDT
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  WSIDF
     END-IF.
*
  220-GRP01-CHECK-11.
*    担当者　（開始）
*    担当者　（終了）
     IF     ( TANT      =    SPACE   ) OR
            ( TANT      =    ZERO    )
              MOVE      "99"       TO   TANT
     END-IF.
 220-GRP01-CHECK-12.
*　　担当者　 範囲チェック
     IF       TANF        >  TANT
              IF    ERR-FLG   =    ZERO
                    MOVE    7      TO   ERR-FLG
                    MOVE   "R"     TO   EDIT-OPTION  OF  TANF
                    MOVE   "R"     TO   EDIT-OPTION  OF  TANT
                    MOVE   "C"     TO   EDIT-CURSOR  OF  TANF
                    GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*    ELSE
*             MOVE  "M"      TO   EDIT-OPTION  OF  TANF
*             MOVE  "M"      TO   EDIT-OPTION  OF  TANT
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  TANF
     END-IF.
*
 220-GRP01-CHECK-13.
*    処理ＮＯ（開始）
     IF     ( SYORIF    NOT  NUMERIC ) OR
            ( SYORIF    =    ZERO    )
              MOVE      ZERO       TO   SYORIF
     END-IF.
 220-GRP01-CHECK-14.
*    処理ＮＯ（終了）
     IF     ( SYORIT    NOT  NUMERIC ) OR
            ( SYORIT    =    ZERO    )
              MOVE      9999999999 TO   SYORIT
     END-IF.
 220-GRP01-CHECK-15.
*　　処理ＮＯ 範囲チェック
     IF       SYORIF      >  SYORIT
              IF    ERR-FLG   =    ZERO
                    MOVE    7      TO   ERR-FLG
                    MOVE   "R"     TO   EDIT-OPTION  OF  SYORIF
                    MOVE   "R"     TO   EDIT-OPTION  OF  SYORIT
                    MOVE   "C"     TO   EDIT-CURSOR  OF  SYORIF
                    GO   TO   220-GRP01-CHECK-EXIT
              END-IF
*    ELSE
*             MOVE  "M"      TO   EDIT-OPTION  OF  TANF
*             MOVE  "M"      TO   EDIT-OPTION  OF  TANT
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  TANF
     END-IF.
*
 220-GRP01-CHECK-16.
*    出力区分 値チェック
     IF     ( OUTKBN    NOT = SPACE  ) AND
            ( OUTKBN    NOT = "1"    )
              IF    ERR-FLG   =    ZERO
                    MOVE    6      TO   ERR-FLG
                    MOVE   "C"     TO   EDIT-CURSOR OF OUTKBN
                    MOVE   "R"     TO   EDIT-OPTION OF OUTKBN
                    GO   TO   220-GRP01-CHECK-EXIT
              END-IF
     END-IF.
*
 220-GRP01-CHECK-17.
*    実行履歴ファイル存在チェック
     MOVE      SPACE          TO   JIK-REC.
     INITIALIZE                    JIK-REC.
     MOVE      KDATEF         TO   JIK-F01.
     MOVE      KTIMEF         TO   JIK-F02.
     MOVE      WSIDF          TO   JIK-F03.
     MOVE      TANF           TO   JIK-F04.
     MOVE      SYORIF         TO   JIK-F05.
     START     JIKRERF   KEY  >=   JIK-F01   JIK-F02
                                   JIK-F03   JIK-F04
                                   JIK-F05   JIK-F06
                                   JIK-F07
          INVALID   KEY
               MOVE      5    TO   ERR-FLG
               MOVE     "C"   TO   EDIT-CURSOR OF KDATEF
               MOVE     "R"   TO   EDIT-OPTION OF KDATEF
               MOVE     "R"   TO   EDIT-OPTION OF KTIMEF
               MOVE     "R"   TO   EDIT-OPTION OF WSIDF
               MOVE     "R"   TO   EDIT-OPTION OF TANF
               MOVE     "R"   TO   EDIT-OPTION OF SYORIF
               GO   TO   220-GRP01-CHECK-EXIT
     END-START.
 220-GRP01-CHECK-18.
*    実行履歴ファイル存在チェック
     READ      JIKRERF
         AT END
               MOVE      5    TO   ERR-FLG
               MOVE     "C"   TO   EDIT-CURSOR OF KDATEF
               MOVE     "R"   TO   EDIT-OPTION OF KDATEF
               MOVE     "R"   TO   EDIT-OPTION OF KDATET
               MOVE     "R"   TO   EDIT-OPTION OF KTIMEF
               MOVE     "R"   TO   EDIT-OPTION OF KTIMET
               MOVE     "R"   TO   EDIT-OPTION OF WSIDF
               MOVE     "R"   TO   EDIT-OPTION OF WSIDT
               MOVE     "R"   TO   EDIT-OPTION OF TANF
               MOVE     "R"   TO   EDIT-OPTION OF TANT
               MOVE     "R"   TO   EDIT-OPTION OF SYORIF
               MOVE     "R"   TO   EDIT-OPTION OF SYORIT
               GO   TO   220-GRP01-CHECK-EXIT
     END-READ.
     IF ( KDATEF    <=   JIK-F01 ) AND
        ( KDATET    >=   JIK-F01 )
          CONTINUE
     ELSE
          MOVE  5   TO   ERR-FLG
          MOVE "C"  TO   EDIT-CURSOR OF KDATEF
          MOVE "R"  TO   EDIT-OPTION OF KDATEF
          MOVE "R"  TO   EDIT-OPTION OF KDATET
          MOVE "R"  TO   EDIT-OPTION OF KTIMEF
          MOVE "R"  TO   EDIT-OPTION OF KTIMET
          MOVE "R"  TO   EDIT-OPTION OF WSIDF
          MOVE "R"  TO   EDIT-OPTION OF WSIDT
          MOVE "R"  TO   EDIT-OPTION OF TANF
          MOVE "R"  TO   EDIT-OPTION OF TANT
          MOVE "R"  TO   EDIT-OPTION OF SYORIF
          MOVE "R"  TO   EDIT-OPTION OF SYORIT
          GO        TO   220-GRP01-CHECK-EXIT
     END-IF.
     IF ( KTIMEF    <=   JIK-F02 ) AND
        ( KTIMET    >=   JIK-F02 )
          CONTINUE
     ELSE
          GO        TO   220-GRP01-CHECK-18
     END-IF.
     IF ( WSIDF     <=   JIK-F03 ) AND
        ( WSIDT     >=   JIK-F03 )
          CONTINUE
     ELSE
          GO        TO   220-GRP01-CHECK-18
     END-IF.
     IF ( TANF      <=   JIK-F04 ) AND
        ( TANT      >=   JIK-F04 )
          CONTINUE
     ELSE
          GO        TO   220-GRP01-CHECK-18
     END-IF.
     IF ( SYORIF    <=   JIK-F05 ) AND
        ( SYORIT    >=   JIK-F05 )
          CONTINUE
     ELSE
          GO        TO   220-GRP01-CHECK-18
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
*
     MOVE     KDATEF              TO   PARA-OUT-KDATEF.
     MOVE     KDATET              TO   PARA-OUT-KDATET.
     MOVE     KTIMEF              TO   PARA-OUT-KTIMEF.
     MOVE     KTIMET              TO   PARA-OUT-KTIMET.
     MOVE     WSIDF               TO   PARA-OUT-WSIDF.
     MOVE     WSIDT               TO   PARA-OUT-WSIDT.
     MOVE     TANF                TO   PARA-OUT-TANF.
     MOVE     TANT                TO   PARA-OUT-TANT.
     MOVE     SYORIF              TO   PARA-OUT-SYORIF.
     MOVE     SYORIT              TO   PARA-OUT-SYORIT.
     MOVE     OUTKBN              TO   PARA-OUT-OUTKBN.
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FCM0010I.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF KDATEF
                                  EDIT-CURSOR OF KDATET
                                  EDIT-CURSOR OF KTIMEF
                                  EDIT-CURSOR OF KTIMET
                                  EDIT-CURSOR OF WSIDF
                                  EDIT-CURSOR OF WSIDT
                                  EDIT-CURSOR OF TANF
                                  EDIT-CURSOR OF TANT
                                  EDIT-CURSOR OF SYORIF
                                  EDIT-CURSOR OF SYORIT
                                  EDIT-CURSOR OF OUTKBN.
     MOVE     "M"            TO   EDIT-OPTION OF KDATEF
                                  EDIT-OPTION OF KDATET
                                  EDIT-OPTION OF KTIMEF
                                  EDIT-OPTION OF KTIMET
                                  EDIT-OPTION OF WSIDF
                                  EDIT-OPTION OF WSIDT
                                  EDIT-OPTION OF TANF
                                  EDIT-OPTION OF TANT
                                  EDIT-OPTION OF SYORIF
                                  EDIT-OPTION OF SYORIT
                                  EDIT-OPTION OF OUTKBN.
*
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
