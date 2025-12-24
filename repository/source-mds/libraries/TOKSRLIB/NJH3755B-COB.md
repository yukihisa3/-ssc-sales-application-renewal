# NJH3755B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH3755B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコＥＤＩ　　　　　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　累積データ抽出　　                *
*    作成日／作成者　　　：　2019/12/04 INOUE                  *
*    処理概要　　　　　　：　ナフコ発注累積データより、条件に　*
*                            合致するレコードを抽出する。　　　*
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH3755B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/12/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注累積データ
     SELECT   NFHACL3   ASSIGN    TO        DA-01-VI-NFHACL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NFH-A25
                                            NFH-A44
                                            NFH-A83
                        FILE  STATUS   IS   NFH-STS.
*発注一覧ワーク
     SELECT   NFWKXXX4  ASSIGN    TO        DA-01-VI-NFWKXXX4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       NFW-A11
                                            NFW-A25
                                            NFW-A26
                                            NFW-A23
                                            NFW-A24
                        FILE  STATUS   IS   NFW-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    発注累積データ
******************************************************************
 FD  NFHACL3            LABEL RECORD   IS   STANDARD.
     COPY     NFHACL3   OF        XFDLIB
              JOINING   NFH       PREFIX.
******************************************************************
*    発注一覧ワーク
******************************************************************
 FD  NFWKXXX4           LABEL RECORD   IS   STANDARD.
     COPY     NFWKXXX4  OF        XFDLIB
              JOINING   NFW       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  WRITE-CNT               PIC  9(07)     VALUE  ZERO.
 01  REWRITE-CNT             PIC  9(07)     VALUE  ZERO.
*
*システム日付の編集
 01  SYS-WORKAREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  STS-AREA.
     03  NFH-STS           PIC  X(02).
     03  NFW-STS           PIC  X(02).
*
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA3.
     03  LI-KBN                  PIC  9.
     03  LI-KETA                 PIC  9.
     03  LI-START                PIC  9(09).
     03  LI-END                  PIC  9(09).
     03  LI-DENNO                PIC  9(09).
     03  L3-ERR                  PIC  9(01).
     03  L3-NEXT                 PIC  9(09).
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH3755B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3755B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3755B".
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
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 01  WORK-AREA.
     03  WK-CD               PIC  9(04).
     03  WK-CALC             PIC  9(01).
**** 03  WK-CALC-2           PIC  9(02).
     03  WK-DIV-1            PIC  9(03).
     03  WK-DIV-2            PIC  9(03).
     03  WK-KETA             PIC  9(01).
     03  CALC-CD             PIC  9(01).
     03  WK-DENNO            PIC  9(09).
     03  WK-DENCHK.
         05  WK-DENCHK1      PIC  9(08).
         05  WK-DENCHK2      PIC  9(01).
 LINKAGE                SECTION.
 01  PARA-IN-HDATE-FROM  PIC   9(08).
 01  PARA-IN-HDATE-TO    PIC   9(08).
 01  PARA-IN-JANCD       PIC   9(13).
 01  PARA-IN-TENPO-FROM  PIC   9(03).
 01  PARA-IN-TENPO-TO    PIC   9(03).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING  PARA-IN-HDATE-FROM
                                        PARA-IN-HDATE-TO
                                        PARA-IN-JANCD
                                        PARA-IN-TENPO-FROM
                                        PARA-IN-TENPO-TO.
*
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHACL3.
     MOVE      "NFHACL3"    TO   AB-FILE.
     MOVE      NFH-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFWKXXX4.
     MOVE      "NFWKXXX4"   TO   AB-FILE.
     MOVE      NFW-STS      TO   AB-STS.
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
*
     OPEN     INPUT     NFHACL3
              I-O       NFWKXXX4.
*
     DISPLAY  MSG-START UPON CONS.
*T
     DISPLAY  "PARA-IN-HDATE-FROM=" PARA-IN-HDATE-FROM UPON CONS.
     DISPLAY  "PARA-IN-HDATE-TO  =" PARA-IN-HDATE-TO   UPON CONS.
     DISPLAY  "PARA-IN-JANCD     =" PARA-IN-JANCD      UPON CONS.
     DISPLAY  "PARA-IN-TENPO-FROM=" PARA-IN-TENPO-FROM UPON CONS.
     DISPLAY  "PARA-IN-TENPO-TO  =" PARA-IN-TENPO-TO   UPON CONS.
*T
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
*ファイルスタート
     PERFORM  INSTART-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃"  UPON CONS
          DISPLAY NC"＃＃　対象データなし　＃＃"  UPON CONS
          DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*ファイル読込
     PERFORM INREAD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    発注累積データスタート
****************************************************************
 INSTART-SEC                SECTION.
*
     MOVE    "INSTART-SEC"        TO   S-NAME.
*
     MOVE     SPACE               TO   NFH-REC.
     INITIALIZE                        NFH-REC.
*
     MOVE     PARA-IN-HDATE-FROM  TO   NFH-A25.
*--- MOVE     PARA-IN-JANCD       TO   NFH-A44.
     MOVE     ZERO                TO   NFH-A44.
*--- MOVE     PARA-IN-TENPO-FROM  TO   NFH-A83.
     MOVE     ZERO                TO   NFH-A83.
*
     START  NFHACL3  KEY  IS  >=  NFH-A25 NFH-A44 NFH-A83
*
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 INSTART-EXIT.
     EXIT.
*
****************************************************************
*   発注累積データ読込
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     READ     NFHACL3  NEXT
         AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
*
 INREAD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-001.
*対象チェック
*  発注日
     IF   NFH-A25    >   PARA-IN-HDATE-TO
          MOVE  "END"    TO     END-FLG
          GO             TO     MAIN-EXIT
     END-IF.
*  ＪＡＮコード
     IF   PARA-IN-JANCD  NOT =  ZERO
          IF   NFH-A44   NOT =  PARA-IN-JANCD
               GO        TO     MAIN-200
          END-IF
     END-IF.
*  店舗コード
     IF ( NFH-A83    <   PARA-IN-TENPO-FROM ) OR
        ( NFH-A83    >   PARA-IN-TENPO-TO   )
          GO             TO     MAIN-200
     END-IF.
*  管理番号（未採番が対象）
     IF   NFH-AH09   NOT = ZERO
          GO             TO     MAIN-200
     END-IF.
*
 MAIN-002.
*ワ－ク存在チェック
     MOVE    NFH-A11            TO   NFW-A11.
     MOVE    NFH-A25            TO   NFW-A25.
     MOVE    NFH-A26            TO   NFW-A26.
     MOVE    NFH-A23            TO   NFW-A23.
     MOVE    NFH-A24            TO   NFW-A24.
     READ    NFWKXXX4
        INVALID
             GO                 TO   MAIN-003
        NOT INVALID
             GO                 TO   MAIN-004
     END-READ.
*ワ－ク出力
 MAIN-003.
     MOVE    NFH-REC            TO   NFW-REC.
     WRITE   NFW-REC.
     ADD     1                  TO   WRITE-CNT.
     GO                         TO   MAIN-200.
*ワ－ク置換
 MAIN-004.
     MOVE    NFH-REC            TO   NFW-REC.
     REWRITE NFW-REC.
     ADD     1                  TO   REWRITE-CNT.
*
 MAIN-200.
     PERFORM  INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数出力
     DISPLAY "NFHACL3  READ    CNT = " READ-CNT    UPON CONS.
     DISPLAY "NFWKXXX4 WRITE   CNT = " WRITE-CNT   UPON CONS.
     DISPLAY "NFWKXXX4 REWRITE CNT = " REWRITE-CNT UPON CONS.
*
     CLOSE     NFHACL3  NFWKXXX4.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
