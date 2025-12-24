****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営利種子ストックＮＯ管理　　　　　*
*    業務名　　　　　　　：　営利種子ストックＮＯ管理          *
*    モジュール名　　　　：　ストックＮＯ管理データ抽出　　    *
*    作成日／更新日　　　：　2020/07/13                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*　　更新日／更新者　　　：　                                  *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SST0010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          20/07/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN              DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        STATUS              MEI-STATUS.
*ストックＮＯ管理ファイル
     SELECT   STNOKRF   ASSIGN    TO        DA-01-VI-STNOKRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       STN-F01   STN-F02
                                            STN-F03   STN-F04
                                            STN-F05   STN-F06
                                            STN-F07   STN-F08
                        FILE      STATUS    STN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
******************************************************************
*    ストックＮＯ管理ファイル
******************************************************************
 FD  STNOKRF            LABEL RECORD   IS   STANDARD.
     COPY     STNOKRF   OF        XFDLIB
              JOINING   STN       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  STRW-CNT                PIC  9(08)     VALUE  ZERO.
 01  STWT-CNT                PIC  9(08)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  STNOKRF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  STN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SST0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SST0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SST0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TOKCD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   STNOKRF.
     MOVE      "STNOKRL1"   TO   AB-FILE.
     MOVE      STN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY "# STN-F01 = " STN-F01     UPON CONS.
     DISPLAY "# STN-F02 = " STN-F02     UPON CONS.
     DISPLAY "# STN-F03 = " STN-F03     UPON CONS.
     DISPLAY "# STN-F04 = " STN-F04     UPON CONS.
     DISPLAY "# STN-F05 = " STN-F05     UPON CONS.
     DISPLAY "# STN-F06 = " STN-F06     UPON CONS.
     DISPLAY "# STN-F07 = " STN-F07     UPON CONS.
     DISPLAY "# STN-F08 = " STN-F08     UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
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
     MOVE     SYS-TIME       TO   SYS-TIME2.
     DISPLAY  "*** SST0020I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SHTDENLA  HMEIMS.
     OPEN     I-O       STNOKRF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TOKCD     TO   DEN-F01.
     MOVE     PARA-SOKO      TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE   "END"   TO   END-FLG
              DISPLAY NC"＃対象ＤＴ無１＃" UPON CONS
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     PERFORM  SHTDENF-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃対象ＤＴ無２＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     READ     SHTDENLA
              AT END    MOVE     "END"      TO  END-FLG
                        GO                  TO  SHTDENF-READ-EXIT
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
*
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " RD-CNT    UPON CONS
     END-IF.
 SHTDENF-READ-010.
*バッチＮＯ確認
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TOKCD     =    DEN-F01 )
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO        TO        SHTDENF-READ-EXIT
     END-IF.
 SHTDENF-READ-020.
*倉庫ＣＤチェック
     IF PARA-SOKO NOT =  SPACE
         IF   PARA-SOKO  NOT =  DEN-F48
              GO          TO   SHTDENF-READ-SEC
         END-IF
     END-IF.
 SHTDENF-READ-030.
*納品日チェック
     IF  PARA-NOUDT  NOT =  ZERO
         IF   PARA-NOUDT  NOT =  DEN-F112
              GO          TO   SHTDENF-READ-SEC
         END-IF
     END-IF.
 SHTDENF-READ-040.
*ストックＮＯ管理区分チェック
     PERFORM  HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG  =  "INV"
         GO               TO   SHTDENF-READ-SEC
     ELSE
         IF  MEI-F97  NOT = 1
             GO           TO   SHTDENF-READ-SEC
         END-IF
     END-IF.
*
 SHTDENF-READ-050.
     MOVE    1            TO   WRT-CNT.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    ストックＮＯ管理ファイル存在チェック
     MOVE     DEN-F46             TO   STN-F01.
     MOVE     DEN-F47             TO   STN-F02.
     MOVE     DEN-F01             TO   STN-F03.
     MOVE     DEN-F48             TO   STN-F04.
     MOVE     DEN-F112            TO   STN-F05.
     MOVE     DEN-F07             TO   STN-F06.
     MOVE     DEN-F02             TO   STN-F07.
     MOVE     DEN-F03             TO   STN-F08.
     PERFORM  STNOKRF-READ-SEC.
     IF  STNOKRF-INV-FLG  =  SPACE  *>存在する必要項目のみセット
         MOVE DEN-F15             TO   STN-F14
         MOVE DEN-F172            TO   STN-F15
         MOVE DEN-F173            TO   STN-F16
         MOVE DEN-F181            TO   STN-F17
         MOVE DEN-F182            TO   STN-F18
         MOVE SYS-DATEW           TO   STN-F98
         MOVE SYS-TIMEW           TO   STN-F99
         REWRITE  STN-REC
         ADD  1                   TO   STRW-CNT
         GO                       TO   MAIN-010
     END-IF.
*ストックＮＯ管理ファイル出力
     MOVE     SPACE               TO   STN-REC.
     INITIALIZE                        STN-REC.
*    バッチ日付
     MOVE     DEN-F46             TO   STN-F01.
*    バッチ時刻
     MOVE     DEN-F47             TO   STN-F02.
*    バッチ取引先
     MOVE     DEN-F01             TO   STN-F03.
*    倉庫ＣＤ
     MOVE     DEN-F48             TO   STN-F04.
*    納品日
     MOVE     DEN-F112            TO   STN-F05.
*    店舗ＣＤ　
     MOVE     DEN-F07             TO   STN-F06.
*    伝票番号
     MOVE     DEN-F02             TO   STN-F07.
*    行番号
     MOVE     DEN-F03             TO   STN-F08.
*    発注日
     MOVE     DEN-F111            TO   STN-F09.
*    出荷日
     MOVE     DEN-F113            TO   STN-F10.
*    サカタ商品ＣＤ
     MOVE     DEN-F1411           TO   STN-F11.
*    サカタ品単ＣＤ
     MOVE     DEN-F1412           TO   STN-F12.
*    発注数量
     MOVE     DEN-F50             TO   STN-F13.
*    出荷数量　
     MOVE     DEN-F15             TO   STN-F14.
*    原価単価
     MOVE     DEN-F172            TO   STN-F15.
*    売価単価
     MOVE     DEN-F173            TO   STN-F16.
*    原価金額
     MOVE     DEN-F181            TO   STN-F17.
*    売価金額
     MOVE     DEN-F182            TO   STN-F18.
*    相手商品ＣＤ
     MOVE     DEN-F25             TO   STN-F19.
*    登録日
     MOVE     SYS-DATEW           TO   STN-F96.
*    登録時刻
     MOVE     SYS-TIMEW           TO   STN-F97.
     WRITE    STN-REC.
     ADD      1                   TO   STWT-CNT.
*
 MAIN-010.
*
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
     DISPLAY  "## REWRITE-CNT = " STRW-CNT UPON CONS.
     DISPLAY  "## WRITE-CNT     " STWT-CNT UPON CONS.
*
     CLOSE     SHTDENLA  HMEIMS  STNOKRF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称マスタ読込　　　　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 HMEIMS-READ-SEC        SECTION.
*
     MOVE    DEN-F1411    TO   MEI-F011.
     MOVE    DEN-F1412    TO   MEI-F012.
     READ  HMEIMS  INVALID     MOVE  "INV"  TO  HMEIMS-INV-FLG
                   NOT INVALID MOVE  SPACE  TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ストックＮＯ管理ファイル読込　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 STNOKRF-READ-SEC       SECTION.
*
     READ  STNOKRF INVALID     MOVE  "INV"  TO  STNOKRF-INV-FLG
                   NOT INVALID MOVE  SPACE  TO  STNOKRF-INV-FLG
     END-READ.
*
 STNOKRF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
