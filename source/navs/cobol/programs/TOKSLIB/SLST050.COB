***************************************************************
*                                                             *
*        条件ファイルリスト    ＳＬＳＴ０５０
*                                                             *
***************************************************************
******************************************************************
*                                                                *
*             IDENTIFICATION      DIVISION                       *
*                                                                *
******************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            OLST050.
 AUTHOR.                K-SHIMIZU.
 DATE-WRITTEN.          91/08/14.
******************************************************************
*                                                                *
*             ENVIRONMENT         DIVISION                       *
*                                                                *
******************************************************************
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150.
 OBJECT-COMPUTER.       K-150.
 SPECIAL-NAMES.
         YA      IS     PITCH-2
         YB      IS     PITCH-15
         YB-21   IS     BAIKAKU
         STATION IS     STA.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---------   条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    JYO-ST
                        RECORD    KEY       JYO-F01  JYO-F02.
*---------   プリンター　ファイル
     SELECT   PRTFILE   ASSIGN    TO        LP-04.
*--< 画面ファイル >--*
     SELECT  DSPFILE    ASSIGN  TO  01-GS-DSPF
                        SYMBOLIC  DESTINATION IS  "DSP"
                        DESTINATION-1         IS  DSP-WS
                        FORMAT                IS  DSP-FMT
                        GROUP                 IS  DSP-GRP
                        PROCESSING  MODE      IS  DSP-PRO
                        UNIT        CONTROL   IS  DSP-UNIT
                        SELECTED    FUNCTION  IS  DSP-FNC
                        FILE        STATUS    IS  DSP-ST.
******************************************************************
*                                                                *
*             DATA                DIVISION                       *
*                                                                *
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*------   条件ファイル
 FD  HJYOKEN  BLOCK     CONTAINS  6    RECORDS
     LABEL    RECORD    IS        STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
     JOINING  JYO       AS        PREFIX.
*------   ﾌﾟﾘﾝﾀ   ﾌｧｲﾙ
 FD  PRTFILE
     LABEL    RECORD    IS        OMITTED
     LINAGE   IS   66.
 01  PRTF-REC.
    03  FILLER          PIC   X(140).
*--------------------------<< ＤＩＳＰＬＡＹ >>
 FD  DSPFILE.
*
     COPY   FL050  OF   XMDLIB.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
 01  DSP-AREA.
     03  DSP-FMT             PIC  X(08)  VALUE   SPACE.
     03  DSP-GRP             PIC  X(08)  VALUE   SPACE.
     03  DSP-WS              PIC  X(08)  VALUE   SPACE.
     03  DSP-WSR  REDEFINES  DSP-WS.
         05  DSP-WS1         PIC  X(02).
         05  DSP-WS2         PIC  9(03).
         05  DSP-WS3         PIC  X(01).
     03  DSP-PRO             PIC  X(02)  VALUE   SPACE.
     03  DSP-UNIT            PIC  X(06)  VALUE   SPACE.
     03  DSP-FNC             PIC  X(04)  VALUE   SPACE.
     03  DSP-ST              PIC  X(02)  VALUE   SPACE.
     03  DSP-ST1             PIC  X(04)  VALUE   SPACE.
 01  MSG-AREA.
     03  MSG01.
         05  FILLER          PIC  N(20)  VALUE
           NC"範囲エラーです！".
     03  MSG02.
         05  FILLER          PIC  N(20)  VALUE
           NC"対象データありません。".
     03  MSG03.
         05  FILLER          PIC  N(20)  VALUE
           NC"リスト出力中".
 01  WOAK-ERIA.
     03  CNT-PAGE            PIC   9(03).
     03  MSG-FLG             PIC   9(01).
     03  MI-FLG              PIC   9(01).
     03  END-FLG             PIC   9(01).
     03  IN-DATA             PIC   X(01).
     03  JYO-ST              PIC   X(02).
     03  JYO-ST2             PIC   X(04).
     03  WK-KEY1.
         05  WK-R1           PIC   9(02).
         05  WK-R2           PIC   X(06).
     03  WK-KEY2.
         05  WK-R4           PIC   9(02).
         05  WK-R5           PIC   X(06).
     03  FILE-ERR001         PIC   N(10)    VALUE
                         NC"条件ファイル異常！".
     03  FILE-ERR002         PIC   N(10)    VALUE
                         NC"画面ファイル異常！".
*01  SISTEM-HIZUKE.
*    03  SYS-DATE            PIC   9(06).
*    03  SYS-DATE-R  REDEFINES  SYS-DATE.
*        05  SYS-YY          PIC   9(02).
*        05  SYS-MM          PIC   9(02).
*        05  SYS-DD          PIC   9(02).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*------  ﾌﾟﾘﾝﾀ  出力用
 01  MEISAI-1.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-3           PIC   9(02).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-4           PIC   X(08).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  FILLER   CHARACTER  TYPE  PITCH-15.
         05  JO-5       PIC   N(20).
         05  DMY        PIC   N(01).
     03  FILLER         PIC   X(09)   VALUE SPACE.
     03  JO-6           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-7           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-8           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-9           PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-10          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-11          PIC   ZZZZZZZZZ.ZZ-.
 01  MEISAI-2.
     03  FILLER         PIC   X(13)   VALUE SPACE.
     03  FILLER   CHARACTER  TYPE  PITCH-15.
         05  JO-15      PIC   N(05).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-16          PIC   X(15).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-17          PIC   X(15).
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-12          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-13          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-14          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-18          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-19          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(01)   VALUE SPACE.
     03  JO-20          PIC   ZZZZZZZZZ.ZZ-.
     03  FILLER         PIC   X(07)   VALUE SPACE.
 01  OMIDASHI.
    03  FILLER       PIC   X(27)    VALUE  SPACE.
    03  FILLER  CHARACTER  TYPE   BAIKAKU.
       05  FILLER    PIC   N(07)    VALUE
                     NC"＊＊　条　件　".
       05  FILLER    PIC   N(16)    VALUE
                     NC"フ　ァ　イ　ル　リ　ス　ト　＊＊".
    03  FILLER       PIC   X(14)    VALUE  SPACE.
    03  OMI-YY       PIC   Z9.
    03  FILLER       PIC   X(01)    VALUE  "/".
    03  OMI-MM       PIC   Z9.
    03  FILLER       PIC   X(01)    VALUE  "/".
    03  OMI-DD       PIC   Z9.
    03  FILLER  CHARACTER  TYPE   PITCH-2.
       05  FILLER    PIC   X(01)    VALUE  SPACE.
       05  FILLER    PIC   N(02)    VALUE  NC"作成".
       05  FILLER    PIC   X(02)    VALUE  SPACE.
       05  OMI-PAGE  PIC   ZZZ9.
       05  FILLER    PIC   X(01)    VALUE  SPACE.
       05  FILLER    PIC   N(01)    VALUE  NC"頁".
 01  KOMIDASHI-1.
    03  FILLER       PIC   X(01)    VALUE  SPACE.
    03  FILLER       PIC   X(04)    VALUE  "KEY1".
    03  FILLER       PIC   X(01)    VALUE  SPACE.
    03  FILLER       PIC   X(04)    VALUE  "KEY2".
    03  FILLER       PIC   X(03)    VALUE  SPACE.
    03  FILLER  CHARACTER  TYPE   PITCH-2.
       05  FILLER    PIC   N(04)    VALUE  NC"日本語１".
       05  FILLER    PIC   X(38)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字１".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字２".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字３".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字４".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字５".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字６".
 01  KOMIDASHI-2.
    03  FILLER       PIC   X(13)    VALUE  SPACE.
    03  FILLER  CHARACTER  TYPE   PITCH-2.
       05  FILLER    PIC   N(04)    VALUE  NC"日本語２".
       05  FILLER    PIC   X(01)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"英数字".
       05  FILLER    PIC   X(10)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"英数字".
       05  FILLER    PIC   X(15)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字７".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字８".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字９".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字Ａ".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字Ｂ".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
       05  FILLER    PIC   N(03)    VALUE  NC"数字Ｃ".
       05  FILLER    PIC   X(08)    VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
*                                                                *
*             PROCEDURE           DIVISION                       *
*                                                                *
******************************************************************
 PROCEDURE              DIVISION.
*------  エラー処理
 DECLARATIVES.
 000-JYO-ERR            SECTION.
     USE  AFTER    EXCEPTION      PROCEDURE    HJYOKEN.
     DISPLAY       JYO-ST         UPON  STA.
     DISPLAY       FILE-ERR001    UPON  STA.
     ACCEPT        IN-DATA        FROM  STA.
     STOP  RUN.
 000-DSP-ERR            SECTION.
     USE  AFTER    EXCEPTION      PROCEDURE    DSPFILE.
     DISPLAY       DSP-ST         UPON  STA.
     DISPLAY       FILE-ERR002    UPON  STA.
     ACCEPT        IN-DATA        FROM  STA.
     STOP  RUN.
 END DECLARATIVES.
****************************************************************
*        処理                                                  *
****************************************************************
 SHORI-SEC              SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL     END-FLG   =   9.
     PERFORM  END-SEC.
     STOP     RUN.
 SHORI-EXIT.
     EXIT.
****************************************************************
*                      初期処理                                *
****************************************************************
 INIT-SEC               SECTION.
*------  ﾌｧｲﾙ  OPEN
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    PRTFILE.
     OPEN     I-O       DSPFILE.
*------  変数  ｸﾘｱ
     MOVE     ZERO      TO   MI-FLG  CNT-PAGE  END-FLG  MSG-FLG.

*------  ｼｽﾃﾑ日付をとる
*****ACCEPT   SYS-DATE  FROM      DATE.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE     WK-Y      TO        OMI-YY.
     MOVE     WK-M      TO        OMI-MM.
     MOVE     WK-D      TO        OMI-DD.
*--< 範囲指定　入力 >--*
     PERFORM      DSP-SEC.
*-----  初期読み
     READ     HJYOKEN   AT END
                        MOVE      9    TO   END-FLG.
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE     JYO-F01     TO   WK-R1.
     MOVE     JYO-F02     TO   WK-R2.
     IF   WK-KEY1   >    WK-KEY2
         IF   MSG-FLG   =    ZERO
              MOVE      MSG02          TO   W00001
              MOVE      "GMSG"         TO   DSP-GRP
              PERFORM   DSP-WR-SEC     THRU DSP-WR-EXIT
         END-IF
              MOVE      9              TO   END-FLG
              GO                       TO   MAIN-EXIT.
     IF   MI-FLG    =   ZERO
        THEN
              PERFORM   MIDASI-SEC.
     IF   LINAGE-COUNTER  >  62
              MOVE      SPACE          TO   PRTF-REC
              WRITE     PRTF-REC    AFTER   PAGE
              PERFORM   MIDASI-SEC.
     PERFORM  MEISAI-SEC.
*
     READ     HJYOKEN   AT END
                        MOVE      9    TO   END-FLG.
 MAIN-EXIT.
      EXIT.
****************************************************************
*                       ＥＮＤ処理                             *
****************************************************************
 END-SEC           SECTION.
     CLOSE    HJYOKEN    PRTFILE     DSPFILE.
 END-EXIT.
     EXIT.
****************************************************************
*                       明細　印字                             *
****************************************************************
 MEISAI-SEC             SECTION.
     MOVE     JYO-F01          TO        JO-3.
     MOVE     JYO-F02          TO        JO-4.
     MOVE     JYO-F03          TO        JO-5.
     MOVE     SPACE            TO        DMY.
     MOVE     JYO-F04          TO        JO-6.
     MOVE     JYO-F05          TO        JO-7.
     MOVE     JYO-F06          TO        JO-8.
     MOVE     JYO-F07          TO        JO-9.
     MOVE     JYO-F08          TO        JO-10.
     MOVE     JYO-F09          TO        JO-11.
     MOVE     JYO-F10          TO        JO-12.
     MOVE     JYO-F11          TO        JO-13.
     MOVE     JYO-F12          TO        JO-14.
     MOVE     JYO-F12A         TO        JO-18.
     MOVE     JYO-F12B         TO        JO-19.
     MOVE     JYO-F12C         TO        JO-20.
     MOVE     JYO-F13          TO        JO-15.
     MOVE     JYO-F14          TO        JO-16.
     MOVE     JYO-F15          TO        JO-17.
     WRITE    PRTF-REC       FROM      MEISAI-1     AFTER   2.
     WRITE    PRTF-REC       FROM      MEISAI-2     AFTER   1.
 MEISAI-EXIT.
     EXIT.
****************************************************************
*                       見出し                                 *
****************************************************************
 MIDASI-SEC             SECTION.
     MOVE     MSG03          TO        W00001
     MOVE     "GMSG"         TO        DSP-GRP
     PERFORM  DSP-WR-SEC     THRU      DSP-WR-EXIT
     COMPUTE  CNT-PAGE   =   CNT-PAGE   +   1.
     MOVE     CNT-PAGE       TO        OMI-PAGE.
     WRITE    PRTF-REC       FROM      OMIDASHI     AFTER   2.
     WRITE    PRTF-REC       FROM      KOMIDASHI-1  AFTER   2.
     WRITE    PRTF-REC       FROM      KOMIDASHI-2  AFTER   1.
     MOVE        1           TO        MI-FLG    MSG-FLG.
 MIDASI-EXIT.
     EXIT.
****************************************************************
*    画面　　処理                                              *
****************************************************************
 DSP-SEC             SECTION.
 DSP-010.
     PERFORM  DSP-INIT-SEC   THRU      DSP-INIT-EXIT.
*
 DSP-020.
     MOVE     "GRP010"       TO        DSP-GRP.
     PERFORM  DSP-RD-SEC     THRU      DSP-RD-EXIT.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                             GO        TO   DSP-010
         WHEN      "F005"
                             MOVE 9    TO   END-FLG
                             GO        TO   DSP-EXIT
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             GO        TO   DSP-020
     END-EVALUATE.
     PERFORM  DSP-WR-SEC     THRU      DSP-WR-EXIT.
*--< ＣＨＫ >--*
     IF     ( R0004    =    ZERO  )  AND
            ( R0005    =    SPACE )
              MOVE     99              TO   R0004
              MOVE     999999          TO   R0005.
     PERFORM  DSP-WR-SEC     THRU      DSP-WR-EXIT.
     MOVE     R0001          TO        WK-R1.
     MOVE     R0002          TO        WK-R2.
     MOVE     R0004          TO        WK-R4.
     MOVE     R0005          TO        WK-R5.
     IF    WK-KEY1  >  WK-KEY2
              MOVE      MSG01          TO   W00001
              MOVE     "GMSG"          TO   DSP-GRP
              PERFORM   DSP-WR-SEC     THRU DSP-WR-EXIT
              GO                       TO   DSP-020.
     MOVE   SPACE            TO        W00001.
     MOVE   "GMSG"           TO        DSP-GRP.
     PERFORM  DSP-WR-SEC     THRU      DSP-WR-EXIT.
 DSP-030.
*--< 確認 >--*
     MOVE   "GRPENT"         TO        DSP-GRP.
     PERFORM  DSP-RD-SEC     THRU      DSP-RD-EXIT.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                             GO        TO   DSP-010
         WHEN      "F005"
                             MOVE 9    TO   END-FLG
                             GO        TO   DSP-EXIT
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             GO        TO   DSP-030
     END-EVALUATE.
*
     MOVE   R0001           TO         JYO-F01.
     MOVE   R0002           TO         JYO-F02.
     START  HJYOKEN  KEY IS  >=  JYO-F01  JYO-F02
        INVALID    KEY
              MOVE      MSG02          TO   W00001
              MOVE      "GMSG"         TO   DSP-GRP
              PERFORM   DSP-WR-SEC     THRU DSP-WR-EXIT
              GO                       TO   DSP-020
        NOT INVALID   KEY
            MOVE        R0004          TO   WK-R4
            MOVE        R0005          TO   WK-R5
     END-START.
     MOVE    "GMSG"          TO        DSP-GRP.
     PERFORM  DSP-WR-SEC     THRU      DSP-WR-EXIT.
*
 DSP-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　                                      *
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE    "NE"            TO        DSP-PRO.
     READ     DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面　　ＷＲＩＴＥ　                                      *
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE     SPACE          TO        DSP-PRO.
     WRITE    FL050.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                            *
****************************************************************
 DSP-INIT-SEC           SECTION.
     INITIALIZE              FL050.
     MOVE     SPACE          TO   FL050.
     MOVE     SPACE          TO   DSP-AREA.
     MOVE    "FL050"         TO   DSP-FMT.
     MOVE    "SCREEN"        TO   DSP-GRP.
     MOVE    "CL"            TO   DSP-PRO.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     WRITE    FL050.
     MOVE     ZERO           TO   R0001   R0004.
     MOVE     SPACE          TO   R0002   R0005.
 DSP-INIT-EXIT.
     EXIT.
*
*    ENDCOBOL.
