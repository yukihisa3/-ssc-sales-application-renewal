****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定エラーリスト　　　　　　　*
*    作成日／作成者　　　：　2022/07/26   INOUE                *
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    流用　　　　　　　　：　NVD0120L                          *
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVD0810L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/07/26.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     YA-12     IS   YA-12
     CONSOLE   IS   CONS.
******************************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
******************************************************************
*ＮＡＶＳ受信データ管理ファイルL1
     SELECT  DNDTKNL1 ASSIGN        TO  DNDTKNL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  DYNAMIC
                      RECORD KEY    IS  DND1-F01 *> 送受信指示NO
                                        DND1-F07 *> 取込日付
                                        DND1-F08 *> 取込時刻
                                        DND1-F09 *> 伝票番号
                                        DND1-F10 *> 行番号
                      FILE   STATUS IS  DND1-STATUS.
*ＮＡＶＳ受信データ管理ファイルL3
     SELECT  DNDTKNL3 ASSIGN        TO  DNDTKNL3
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  DYNAMIC
                      RECORD KEY    IS  DND3-F07 *> 取込日付
                                        DND3-F08 *> 取込時刻
                                        DND3-F09 *> 伝票番号
                                        DND3-F10 *> 行番号
                      FILE   STATUS IS  DND3-STATUS.
*条件ファイル
     SELECT  HJYOKEN  ASSIGN        TO  JYOKEN1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  JYO-F01
                                        JYO-F02
                      FILE   STATUS IS  JYO-STATUS.
*プリンタ
     SELECT   PRTF    ASSIGN        TO  LP-04-PRTF
                      FILE  STATUS  IS  PRT-STATUS.
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ＮＡＶＳ受信データ管理ファイルL1
 FD  DNDTKNL1.
     COPY        DNDTKNL1    OF        XFDLIB
     JOINING     DND1        AS        PREFIX.
*ＮＡＶＳ受信データ管理ファイルL3
 FD  DNDTKNL3.
     COPY        DNDTKNL3    OF        XFDLIB
     JOINING     DND3        AS        PREFIX.
*条件ファイル
 FD  HJYOKEN.
     COPY        HJYOKEN     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  DNDTKNL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  DNDTKNL3-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  HJYOKEN-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  WK-KEISAN               PIC S9(11)V99  VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*ステータス
 01  WK-ST.
     03  DND1-STATUS        PIC  X(02).
     03  DND3-STATUS        PIC  X(02).
     03  JYO-STATUS         PIC  X(02).
     03  PRT-STATUS         PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVD0810L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVD0810L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVD0810L".
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
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".

 01  HD01.
     03  FILLER                      CHARACTER  TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  X(08)   VALUE  "NVD0810L".
         05  FILLER          PIC  X(104)  VALUE  SPACE.
         05  HD1-YYYY        PIC  9(04).
         05  FILLER          PIC  N(01)   VALUE  NC"年".
         05  HD1-MM          PIC  Z9.
         05  FILLER          PIC  N(01)   VALUE  NC"月".
         05  HD1-DD          PIC  Z9.
         05  FILLER          PIC  N(01)   VALUE  NC"日".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  HD1-PCNT        PIC  ZZ9.
         05  FILLER          PIC  N(01)   VALUE  NC"頁".
 01  HD02.
     03  FILLER                      CHARACTER  TYPE YA-21.
         05  FILLER          PIC  X(30)   VALUE  SPACE.
         05  FILLER          PIC  N(12)   VALUE
         NC"（Ｄ３６５連携）＜取込エ".
     03  FILLER                      CHARACTER  TYPE YA-21.
         05  FILLER          PIC  N(02)   VALUE
         NC"ラー".
     03  FILLER                      CHARACTER  TYPE YA-21.
         05  FILLER          PIC  N(04)   VALUE
         NC"リスト＞".
     03  FILLER                      CHARACTER  TYPE YA.
         05  FILLER          PIC  X(13)   VALUE  SPACE.
         05  HD2-HH          PIC  9(02).
         05  FILLER          PIC  N(01)   VALUE  NC"：".
         05  HD2-SS          PIC  9(02).
         05  FILLER          PIC  N(01)   VALUE  NC"：".
         05  HD2-MS          PIC  9(02).
*
 01  HD03.
     03  FILLER                      CHARACTER  TYPE YA.
         05  FILLER          PIC  X(24)   VALUE  SPACE.
         05  FILLER          PIC  N(06)   VALUE
         NC"データ種別：".
         05  HD3-SYUCD       PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD3-SYUNM       PIC  N(10).
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE
         NC"取込日付：".
         05  HD3-DATE        PIC  X(10).
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE
         NC"取込時刻：".
         05  HD3-TIME        PIC  X(08).
*
 01  HD04.
     03  FILLER                      CHARACTER TYPE YB.
         05  FILLER          PIC  X(40)   VALUE  SPACE.
         05  FILLER          PIC  N(25)   VALUE
         NC"エラー＝＞１伝票_重複、２商品未登録、３場所未登録".
*#2020/10/05 NAV ST
*********05  FILLER          PIC  N(16)   VALUE
         05  FILLER          PIC  N(24)   VALUE
*********NC"、_仕入先エラー、_日付エラー、".
         NC"、４仕入先エラー、５日付エラー、６ＪＡＮＣＤ空白".
*#2020/10/05 NAV ED
*#2020/10/05 NAV ST
*********05  FILLER          PIC  N(09)   VALUE
         05  FILLER          PIC  N(11)   VALUE
*********NC"_、_、_、_、_".
         NC"エラー、７、８、９、_".
*#2020/10/05 NAV ED
*
 01  HD05.
     03  FILLER                      CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"伝票番号".
         05  FILLER          PIC  X(13)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"行".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"発注日".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"新商品".
         05  FILLER          PIC  X(15)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"ＪＡＮ".
         05  FILLER          PIC  X(07)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"倉庫".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"入荷数量".
         05  FILLER          PIC  X(09)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"単　価".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(15)   VALUE
         NC"【　　エ　ラ　ー　区　分　　】".
*
 01  HD06.
     03  FILLER                      CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  X(27)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"納品日".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"旧商品".
         05  FILLER          PIC  X(28)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"場所".
         05  FILLER          PIC  X(21)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"金　額".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"１".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"２".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"３".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"４".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"５".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"６".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"７".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"８".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"９".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE   NC"_".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
*
 01  MS01.
     03  FILLER                       CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO      PIC  X(20).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-GYO        PIC  Z9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-HACHI      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-NSYOHIN    PIC  X(20).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-JANCD      PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SOKO       PIC  X(03).
         05  MS01-SURYO      PIC  ----,---,--9.99.
         05  MS01-TANKA      PIC  ----,---,--9.99.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR1       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR2       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR3       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR4       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR5       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR6       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR7       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR8       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR9       PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR10      PIC  N(01).
*
 01  MS02.
     03  FILLER.
         05  FILLER          PIC  X(26)     VALUE  SPACE.
         05  MS02-NOUHI      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-OSYOHIN    PIC  X(20).
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  MS02-BASYO      PIC  X(02).
         05  FILLER          PIC  X(16)     VALUE  SPACE.
         05  MS02-KINGAKU    PIC  ----,---,--9.
*
*対象データなし
 01  LST-DATA-NASI.
     03  FILLER                      CHARACTER TYPE YA-22.
         05  FILLER          PIC  X(40)     VALUE  SPACE.
         05  FILLER          PIC  N(12)     VALUE
             NC"取込エラーはありません。".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".

*入荷予定レコード展開
     COPY        YTINYKF     OF        XFDLIB
     JOINING     YTI         AS        PREFIX.

*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
******************************************************************
 LINKAGE                SECTION.
 01  PARA-IN-SELKBN        PIC   X(01).
 01  PARA-IN-ONLNO         PIC   9(10).
 01  PARA-IN-TDATE         PIC   9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION
                           USING PARA-IN-SELKBN
                                 PARA-IN-ONLNO
                                 PARA-IN-TDATE.
******************************************************************
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNDTKNL1.
     MOVE      "DNDTKNL1"   TO   AB-FILE.
     MOVE      DND1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNDTKNL3.
     MOVE      "DNDTKNL3"   TO   AB-FILE.
     MOVE      DND3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "HJYOKEN1"   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.

*#### TEST <<<<<<
*### DISPLAY " 部門 ------------ "  PARA-IN-BUMON UPON CONS.
*### DISPLAY " 担当者 ---------- "  PARA-IN-TANCD UPON CONS.
*### DISPLAY " 送受信指示Ｎｏ -- "  PARA-IN-ONLNO UPON CONS.
*### DISPLAY "  " UPON CONS.
*#### TEST >>>>>>
*
     DISPLAY  MSG-START UPON CONS.
*
*ファイルＯＰＥＮ
     OPEN     INPUT     DNDTKNL1 DNDTKNL3
                        HJYOKEN.
     OPEN     OUTPUT    PRTF.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT   SYS-TIME          FROM   TIME.
*
*ＮＡＶＳ受信データ管理ファイルL1 スタート
     IF   PARA-IN-SELKBN = "1"
          PERFORM  DNDTKNL1-START-SEC
          IF   END-FLG = "END"
               PERFORM HEAD1-WT-SEC
               WRITE  PRT-REC  FROM  LST-DATA-NASI AFTER  4
               GO                    TO   INIT-EXIT
          END-IF
     END-IF.
*ＮＡＶＳ受信データ管理ファイルL1 読込
     IF   PARA-IN-SELKBN = "1"
     PERFORM DNDTKNL1-READ-SEC
          IF   END-FLG = "END"
               PERFORM HEAD1-WT-SEC
               WRITE  PRT-REC  FROM  LST-DATA-NASI AFTER  4
               GO                    TO   INIT-EXIT
          END-IF
     END-IF.
*
*ＮＡＶＳ受信データ管理ファイルL3 スタート
     IF   PARA-IN-SELKBN = "2"
          PERFORM  DNDTKNL3-START-SEC
          IF   END-FLG = "END"
               PERFORM HEAD1-WT-SEC
               WRITE  PRT-REC  FROM  LST-DATA-NASI AFTER  4
               GO                    TO   INIT-EXIT
          END-IF
     END-IF.
*ＮＡＶＳ受信データ管理ファイルL3 読込
     IF   PARA-IN-SELKBN = "2"
     PERFORM DNDTKNL3-READ-SEC
          IF   END-FLG = "END"
               PERFORM HEAD1-WT-SEC
               WRITE  PRT-REC  FROM  LST-DATA-NASI AFTER  4
               GO                    TO   INIT-EXIT
          END-IF
     END-IF.
*
*ヘッダ印刷
     IF      PARA-IN-SELKBN  =  "1"
             PERFORM HEAD1-WT-SEC
     ELSE
             PERFORM HEAD3-WT-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL1 スタート
****************************************************************
 DNDTKNL1-START-SEC          SECTION.
*
     MOVE    "DNDTKNL1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   DND1-REC.
     INITIALIZE                        DND1-REC.
     MOVE     PARA-IN-ONLNO       TO   DND1-F01.
*
     START  DNDTKNL1  KEY  IS  >=      DND1-F01
                                       DND1-F07
                                       DND1-F08
                                       DND1-F09
                                       DND1-F10
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 DNDTKNL1-START-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL1 読込
****************************************************************
 DNDTKNL1-READ-SEC           SECTION.
*
     MOVE    "DNDTKNL1-READ-SEC"   TO   S-NAME.
*
 DNDTKNL1-READ-010.
     READ     DNDTKNL1       NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL1-READ-EXIT
     END-READ.
*送受信指示NOを判定
     IF       DND1-F01   NOT =  PARA-IN-ONLNO
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL1-READ-EXIT
     END-IF.
*エラー区分チェック
     IF       DND1-F22   NOT =  "1"
              GO                  TO   DNDTKNL1-READ-010
     END-IF.
*件数カウント
     ADD      1                   TO   DNDTKNL1-READ-CNT.
     MOVE     DND1-F20            TO   YTI-REC.
*
 DNDTKNL1-READ-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL3 スタート
****************************************************************
 DNDTKNL3-START-SEC          SECTION.
*
     MOVE    "DNDTKNL3-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   DND3-REC.
     INITIALIZE                        DND3-REC.
     MOVE     PARA-IN-TDATE       TO   DND3-F07.
*
     START  DNDTKNL3  KEY  IS  >=      DND3-F07
                                       DND3-F08
                                       DND3-F09
                                       DND3-F10
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 DNDTKNL3-START-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL3 読込
****************************************************************
 DNDTKNL3-READ-SEC           SECTION.
*
     MOVE    "DNDTKNL3-READ-SEC"   TO   S-NAME.
*
 DNDTKNL3-READ-010.
     READ     DNDTKNL3       NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL3-READ-EXIT
     END-READ.
*取込日付を判定
     IF       DND3-F07   NOT =  PARA-IN-TDATE
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL3-READ-EXIT
     END-IF.
*エラー区分チェック
     IF       DND3-F22   NOT =  "1"
              GO                  TO   DNDTKNL3-READ-010
     END-IF.
*件数カウント
     ADD      1                   TO   DNDTKNL3-READ-CNT.
     MOVE     DND3-F20            TO   YTI-REC.
*
 DNDTKNL3-READ-EXIT.
     EXIT.
*
****************************************************************
*　　メイン処理
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     IF       PARA-IN-SELKBN  =  "1"
              PERFORM  MEISAI1-WT-SEC
              MOVE     SPACE  TO   PRT-REC
     ELSE
              PERFORM  MEISAI3-WT-SEC
              MOVE     SPACE  TO   PRT-REC
     END-IF.
*
 MAIN-020.
     IF       PARA-IN-SELKBN  =  "1"
              PERFORM  DNDTKNL1-READ-SEC
     ELSE
              PERFORM  DNDTKNL3-READ-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字 L1より
*--------------------------------------------------------------*
 HEAD1-WT-SEC            SECTION.
     MOVE    "HEAD1-WT-SEC"        TO   S-NAME.

*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.

*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD1-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD1-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD1-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD1-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD2-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD2-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD2-MS.

     IF  DNDTKNL1-READ-CNT  =  1
*    種別取得
         MOVE   DND1-F04          TO   HD3-SYUCD
         MOVE   87                TO   JYO-F01
         MOVE   DND1-F04          TO   JYO-F02
         PERFORM  HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = SPACE
               MOVE  JYO-F03      TO   HD3-SYUNM
         ELSE
               MOVE  ALL NC"？"   TO   HD3-SYUNM
         END-IF
*    取込日付
         MOVE   DND1-F07(1:4)     TO   HD3-DATE(1:4)
         MOVE   "/"               TO   HD3-DATE(5:1)
         MOVE   DND1-F07(5:2)     TO   HD3-DATE(6:2)
         MOVE   "/"               TO   HD3-DATE(8:1)
         MOVE   DND1-F07(7:2)     TO   HD3-DATE(9:2)
*    取込時刻
         MOVE   DND1-F08(1:2)     TO   HD3-TIME(1:2)
         MOVE   ":"               TO   HD3-TIME(3:1)
         MOVE   DND1-F08(3:2)     TO   HD3-TIME(4:2)
         MOVE   ":"               TO   HD3-TIME(6:1)
         MOVE   DND1-F08(5:2)     TO   HD3-TIME(7:2)
     END-IF.

*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  HD06  AFTER  1.
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     9                   TO    LINE-CNT.
*
 HEAD1-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字 L3より
*--------------------------------------------------------------*
 HEAD3-WT-SEC            SECTION.
     MOVE    "HEAD3-WT-SEC"        TO   S-NAME.

*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.

*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD1-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD1-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD1-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD1-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD2-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD2-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD2-MS.

     IF  DNDTKNL3-READ-CNT  =  1
*    種別取得
         MOVE   DND3-F04          TO   HD3-SYUCD
         MOVE   87                TO   JYO-F01
         MOVE   DND3-F04          TO   JYO-F02
         PERFORM  HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = SPACE
               MOVE  JYO-F03      TO   HD3-SYUNM
         ELSE
               MOVE  ALL NC"？"   TO   HD3-SYUNM
         END-IF
*    取込日付
         MOVE   DND3-F07(1:4)     TO   HD3-DATE(1:4)
         MOVE   "/"               TO   HD3-DATE(5:1)
         MOVE   DND3-F07(5:2)     TO   HD3-DATE(6:2)
         MOVE   "/"               TO   HD3-DATE(8:1)
         MOVE   DND3-F07(7:2)     TO   HD3-DATE(9:2)
*    取込時刻
         MOVE   DND3-F08(1:2)     TO   HD3-TIME(1:2)
         MOVE   ":"               TO   HD3-TIME(3:1)
         MOVE   DND3-F08(3:2)     TO   HD3-TIME(4:2)
         MOVE   ":"               TO   HD3-TIME(6:1)
         MOVE   DND3-F08(5:2)     TO   HD3-TIME(7:2)
     END-IF.

*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  HD06  AFTER  1.
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     9                   TO    LINE-CNT.
*
 HEAD3-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字  L1より
*--------------------------------------------------------------*
 MEISAI1-WT-SEC          SECTION.
     MOVE    "MEISAI1-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF       LINE-CNT  >   55
              PERFORM HEAD1-WT-SEC
     END-IF.
*
*明細１行目／編集
*   伝票番号
     MOVE     DND1-F09            TO   MS01-DENNO.
*   行
     MOVE     DND1-F10            TO   MS01-GYO.
*   発注日
     MOVE     YTI-F04             TO   MS01-HACHI.
*   新商品
     MOVE     YTI-F06             TO   MS01-NSYOHIN.
*   ＪＡＮ
     MOVE     YTI-F08             TO   MS01-JANCD.
*   倉庫
     MOVE     YTI-F15             TO   MS01-SOKO.
*   入荷数量
     IF  YTI-F10 NOT = "-"
         COMPUTE  WK-KEISAN  = YTI-F11 / 100
     ELSE
         COMPUTE  WK-KEISAN  = YTI-F11 / 100 * -1
     END-IF.
     MOVE     WK-KEISAN           TO   MS01-SURYO.
*   単価
     COMPUTE  WK-KEISAN  = YTI-F12 / 100.
     MOVE     WK-KEISAN           TO   MS01-TANKA.
*
*   エラー区分１
     IF  DND1-F23  NOT = "1"
              MOVE SPACE          TO   MS01-ERR1
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR1
     END-IF.
*    エラー区分２
     IF  DND1-F24  NOT = "1"
              MOVE SPACE          TO   MS01-ERR2
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR2
     END-IF.
*    エラー区分３
     IF  DND1-F25  NOT = "1"
              MOVE SPACE          TO   MS01-ERR3
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR3
     END-IF.
*    エラー区分４
     IF  DND1-F26  NOT = "1"
              MOVE SPACE          TO   MS01-ERR4
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR4
     END-IF.
*    エラー区分５
     IF  DND1-F27  NOT = "1"
              MOVE SPACE          TO   MS01-ERR5
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR5
     END-IF.
*    エラー区分６
     IF  DND1-F28  NOT = "1"
              MOVE SPACE          TO   MS01-ERR6
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR6
     END-IF.
*    エラー区分７
     IF  DND1-F29  NOT = "1"
              MOVE SPACE          TO   MS01-ERR7
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR7
     END-IF.
*    エラー区分８
     IF  DND1-F30  NOT = "1"
              MOVE SPACE          TO   MS01-ERR8
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR8
     END-IF.
*    エラー区分９
     IF  DND1-F31  NOT = "1"
              MOVE SPACE          TO   MS01-ERR9
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR9
     END-IF.
*    エラー区分１０
     IF  DND1-F32  NOT = "1"
              MOVE SPACE          TO   MS01-ERR10
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR10
     END-IF.
*
*---------------------------------------------------
*明細２行目／編集
*   納品日
     MOVE     YTI-F05             TO   MS02-NOUHI.
*   旧商品
     MOVE     YTI-F07             TO   MS02-OSYOHIN.
*   場所
     MOVE     YTI-F16             TO   MS02-BASYO.
*   金額
     IF  YTI-F13 NOT = "-"
         MOVE  YTI-F14            TO   WK-KEISAN
     ELSE
         COMPUTE  WK-KEISAN  = YTI-F14 * -1
     END-IF.
     MOVE     WK-KEISAN           TO   MS02-KINGAKU.
*
*---------------------------------------------------
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
*行カウント
     ADD      2                   TO   LINE-CNT.
*
 MEISAI1-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字  L3より
*--------------------------------------------------------------*
 MEISAI3-WT-SEC          SECTION.
     MOVE    "MEISAI3-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF       LINE-CNT  >   55
              PERFORM HEAD3-WT-SEC
     END-IF.
*
*明細１行目／編集
*   伝票番号
     MOVE     DND3-F09            TO   MS01-DENNO.
*   行
     MOVE     DND3-F10            TO   MS01-GYO.
*   発注日
     MOVE     YTI-F04             TO   MS01-HACHI.
*   新商品
     MOVE     YTI-F06             TO   MS01-NSYOHIN.
*   ＪＡＮ
     MOVE     YTI-F08             TO   MS01-JANCD.
*   倉庫
     MOVE     YTI-F15             TO   MS01-SOKO.
*   入荷数量
     IF  YTI-F10 NOT = "-"
         COMPUTE  WK-KEISAN  = YTI-F11 / 100
     ELSE
         COMPUTE  WK-KEISAN  = YTI-F11 / 100 * -1
     END-IF.
     MOVE     WK-KEISAN           TO   MS01-SURYO.
*   単価
     COMPUTE  WK-KEISAN  = YTI-F12 / 100.
     MOVE     WK-KEISAN           TO   MS01-TANKA.
*
*   エラー区分１
     IF  DND3-F23  NOT = "1"
              MOVE SPACE          TO   MS01-ERR1
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR1
     END-IF.
*    エラー区分２
     IF  DND3-F24  NOT = "1"
              MOVE SPACE          TO   MS01-ERR2
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR2
     END-IF.
*    エラー区分３
     IF  DND3-F25  NOT = "1"
              MOVE SPACE          TO   MS01-ERR3
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR3
     END-IF.
*    エラー区分４
     IF  DND3-F26  NOT = "1"
              MOVE SPACE          TO   MS01-ERR4
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR4
     END-IF.
*    エラー区分５
     IF  DND3-F27  NOT = "1"
              MOVE SPACE          TO   MS01-ERR5
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR5
     END-IF.
*    エラー区分６
     IF  DND3-F28  NOT = "1"
              MOVE SPACE          TO   MS01-ERR6
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR6
     END-IF.
*    エラー区分７
     IF  DND3-F29  NOT = "1"
              MOVE SPACE          TO   MS01-ERR7
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR7
     END-IF.
*    エラー区分８
     IF  DND3-F30  NOT = "1"
              MOVE SPACE          TO   MS01-ERR8
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR8
     END-IF.
*    エラー区分９
     IF  DND3-F31  NOT = "1"
              MOVE SPACE          TO   MS01-ERR9
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR9
     END-IF.
*    エラー区分１０
     IF  DND3-F32  NOT = "1"
              MOVE SPACE          TO   MS01-ERR10
     ELSE
              MOVE NC"Ｅ"         TO   MS01-ERR10
     END-IF.
*
*---------------------------------------------------
*明細２行目／編集
*   納品日
     MOVE     YTI-F05             TO   MS02-NOUHI.
*   旧商品
     MOVE     YTI-F07             TO   MS02-OSYOHIN.
*   場所
     MOVE     YTI-F16             TO   MS02-BASYO.
*   金額
     IF  YTI-F13 NOT = "-"
         MOVE  YTI-F14            TO   WK-KEISAN
     ELSE
         COMPUTE  WK-KEISAN  = YTI-F14 * -1
     END-IF.
     MOVE     WK-KEISAN           TO   MS02-KINGAKU.
*
*---------------------------------------------------
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
*行カウント
     ADD      2                   TO   LINE-CNT.
*
 MEISAI3-WT-EXIT.
     EXIT.
****************************************************************
*　　終了処理
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝ＮＡＶＳ受信データ管理ファイル読込件数
     IF   PARA-IN-SELKBN  =  "1"
          DISPLAY "DNDTKNL1 PRINT-CNT = "
                   DNDTKNL1-READ-CNT   UPON CONS
     ELSE
          DISPLAY "DNDTKNL3 PRINT-CNT = "
                   DNDTKNL3-READ-CNT   UPON CONS
     END-IF.
*
     CLOSE     DNDTKNL1 DNDTKNL3  HJYOKEN  PRTF.
*
     DISPLAY  MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    条件ファイル／索引
****************************************************************
 HJYOKEN-READ-SEC           SECTION.
     MOVE "HJYOKEN-READ-SEC"       TO   S-NAME.
*
     READ       HJYOKEN
         INVALID       MOVE "INV" TO   HJYOKEN-INV-FLG
         NOT  INVALID  MOVE SPACE TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
