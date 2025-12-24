# SSE7115I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE7115I.COB`

## ソースコード

```cobol
****************************************************************
*    ジョイフル本田用                                          *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求データ削除　　　　　　　　　　*
*    作成日／更新日　　　：　05/08/17                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求データの登録・修正・削除　　　*
*    処理概要　　　　　　：　を　行う　　　　　　　　　　　　　*
*    作成日／更新日　　　：　17/04/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗４桁対応　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE7115I.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*---<<  請求合計ファイル  >>---*
     SELECT   JHJSEKF   ASSIGN    TO        DA-01-VI-JHJSEKL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SEI-F01
                                                 SEI-F02
                                                 SEI-F03
                        FILE      STATUS    IS   SEI-STATUS.
*
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FSE71151   OF        XMDLIB
              JOINING    DSP       PREFIX.
*---<<  請求合計ファイル  >>---*
 FD  JHJSEKF.
     COPY     JHJSEKF    OF        XFDLIB
              JOINING    SEI       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS     OF        XFDLIB
              JOINING    TEN       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  SEI-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
*
 01  WK-SIME                 PIC  9(06).
 01  WK-SIME-R   REDEFINES   WK-SIME.
     02  WK-SIME01           PIC  9(02).
     02  WK-SIME02           PIC  9(02).
     02  WK-SIME03           PIC  9(02).
*
 01  WK-NOUHIN               PIC  9(06).
 01  WK-NOU-R    REDEFINES   WK-NOUHIN.
     02  WK-NOU01            PIC  9(02).
     02  WK-NOU02            PIC  9(02).
     02  WK-NOU03            PIC  9(02).
 01  WK-SYORI-KBN            PIC  9(01)    VALUE  ZERO.
 01  GYO-CNT                 PIC  9(01)    VALUE  1.
 01  WK-GOKEI                PIC  9(09).
 01  WK-SGOKEI               PIC  9(10).
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
*発注日チェック用
 01  WK-CHKH-DATE.
     03  WK-CHKH-YY               PIC  9(02)  VALUE  ZERO.
     03  WK-CHKH-MM               PIC  9(02)  VALUE  ZERO.
     03  WK-CHKH-DD               PIC  9(02)  VALUE  ZERO.
*納品日チェック用
 01  WK-CHKN-DATE.
     03  WK-CHKN-YY               PIC  9(02)  VALUE  ZERO.
     03  WK-CHKN-MM               PIC  9(02)  VALUE  ZERO.
     03  WK-CHKN-DD               PIC  9(02)  VALUE  ZERO.
*
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消　_再入力".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了".
     02  PMSG03            PIC N(20) VALUE
                           NC"_取消　_終了　_再入力".
     02  PMSG04            PIC N(20) VALUE
                           NC"_取消".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSE7115I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR01            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR02            PIC  N(28)  VALUE
            NC"データが未登録です。".
     02  MSG-ERR03            PIC  N(28) VALUE
            NC"取引先名が未登録です。".
     02  MSG-ERR04            PIC  N(28) VALUE
            NC"店舗名が未登録です。".
     02  MSG-ERR05            PIC  N(28) VALUE
            NC"Ｙで入力して下さい。".
     02  MSG-ERR06            PIC  N(28) VALUE
            NC"既に登録済です。".
     02  MSG-ERR07            PIC  N(28) VALUE
            NC"処理区分が違います。".
     02  MSG-ERR08            PIC  N(28) VALUE
            NC"店舗コード未登録。".
     02  MSG-ERR09            PIC  N(28) VALUE
            NC"請求締日が違います。".
     02  MSG-ERR10           PIC  N(28) VALUE
            NC"納品日が違います。".
     02  MSG-ERR11           PIC  N(28) VALUE
            NC"伝票_が未入力です。".
     02  MSG-ERR12           PIC  N(28) VALUE
            NC"伝票区分エラーです。".
     02  MSG-ERR13           PIC  N(28) VALUE
            NC"伝票番号が未登録です。".
     02  MSG-ERR14            PIC  N(28) VALUE
            NC"取引日付が違います。".
     02  MSG-ERR15            PIC  N(28) VALUE
            NC"部門コードを入力して下さい。".
     02  MSG-ERR16            PIC  N(28) VALUE
            NC"項目コードを入力して下さい。".
     02  MSG-ERR17            PIC  N(28) VALUE
            NC"伝票区分を入力して下さい。".
     02  MSG-ERR18            PIC  N(28) VALUE
            NC"区分エラーです。".
     02  MSG-ERR19            PIC  N(28) VALUE
            NC"請求締日を入力して下さい。".
     02  MSG-ERR20            PIC  N(28) VALUE
            NC"店舗コードを入力して下さい。".
     02  MSG-ERR21            PIC  N(28) VALUE
            NC"伝票番号を入力して下さい。".
     02  MSG-ERR22            PIC  N(28) VALUE
            NC"処理区分を入力して下さい。".
     02  MSG-ERR23            PIC  N(28) VALUE
            NC"取引日付を入力して下さい。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS 50   TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JHJSEKF.
     MOVE     "JHJSEKF"      TO   ERR-FL-ID.
     MOVE     SEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTENMS.
     MOVE     "HTENMS"       TO   ERR-FL-ID.
     MOVE     TEN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 SSE7115I-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SSE7115I-END.
     EXIT.
****************************************************************
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*
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
     OPEN    I-O             DSPF.
     OPEN    I-O             JHJSEKF.
     OPEN    INPUT           HTENMS.
     MOVE    "FSE71151"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FSE71151.
     MOVE    SPACE           TO   END-FLG.
     MOVE    "0"             TO   MAIN-FLG.
     MOVE    SPACE           TO   DSP-PROC.
     MOVE    HEN-DATE        TO   DSP-SDATE.
     MOVE    HEN-TIME        TO   DSP-STIME.
     PERFORM DSP-WRITE-SUB.
 INIT-END.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "0"       PERFORM   KBN-SUB
         WHEN      "1"       PERFORM   HEAD1-SUB
         WHEN      "2"       PERFORM   HEAD2-SUB
         WHEN      "3"       PERFORM   BODY-SUB
         WHEN      "4"       PERFORM   KAKUNIN-SUB
         WHEN      "5"       PERFORM   FILDEL-SUB
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        処理区分入力                                  *
*--------------------------------------------------------------*
 KBN-SUB                     SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG02    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KBN"      TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
*     MOVE    ZERO      TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "E000"
         IF    DSP-KUBUN  NOT =    1 AND 2 AND 3
               MOVE   "0"     TO   MAIN-FLG
               MOVE   "R"     TO
                         EDIT-OPTION  OF  DSP-KUBUN
               MOVE   "C"     TO
                         EDIT-CURSOR  OF  DSP-KUBUN
               IF      DSP-KUBUN   =  SPACE
                       MOVE    19     TO   ERR-MSG-CD
               ELSE
                       MOVE    07     TO   ERR-MSG-CD
               END-IF
          ELSE
               MOVE   "D"     TO
                           EDIT-OPTION  OF  DSP-KUBUN
               MOVE   SPACE   TO
                           EDIT-CURSOR  OF  DSP-KUBUN
               MOVE   "1"     TO MAIN-FLG
         END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
 KBN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ヘッダ1入力　
*--------------------------------------------------------------*
 HEAD1-SUB                    SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "HEAD1"     TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
*
     EVALUATE DSP-FUNC
              WHEN  "F004"
              MOVE   SPACE        TO   DSP-HEAD01
              MOVE   SPACE        TO   DSP-HEAD02
              MOVE   SPACE        TO   DSP-BODY01
              MOVE   "D"          TO   EDIT-OPTION  OF  DSP-TENCD
              MOVE   SPACE        TO   EDIT-CURSOR  OF  DSP-TENCD
              MOVE   "0"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
                                       DSP-SGOKEI
              MOVE   SPACE        TO   DSP-KAKNIN
              GO                  TO   HEAD1-END
              WHEN "E000"
                   PERFORM   HEAD1-CHK-SUB
              WHEN "F005"
                   MOVE      "END"     TO   END-FLG
              WHEN "F009"
                   MOVE      "1"       TO   MAIN-FLG
                   MOVE      SPACE     TO   DSP-HEAD01
                   GO                  TO   HEAD1-SUB
     END-EVALUATE.
*
     IF       ERR-MSG-CD     =    ZERO
              MOVE      "2"  TO   MAIN-FLG
     END-IF.
     IF       MAIN-FLG       =    "2"
     AND      DSP-KUBUN      =     3
              MOVE           "4"       TO   MAIN-FLG
     END-IF.
*
 HEAD1-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ヘッダ1チェック
*--------------------------------------------------------------*
 HEAD1-CHK-SUB                SECTION.
*請求締日チェック
     IF      DSP-SIMEBI     =   ZERO
             MOVE   "1"     TO  MAIN-FLG
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-SIMEBI
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-SIMEBI
             MOVE   19      TO  ERR-MSG-CD
     ELSE
             MOVE  DSP-SIMEBI(3:6)        TO   WK-CHKH-DATE
             MOVE     "3"                 TO   LINK-IN-KBN
             MOVE     WK-CHKH-DATE        TO   LINK-IN-YMD6
             MOVE     ZERO                TO   LINK-IN-YMD8
             MOVE     ZERO                TO   LINK-OUT-RET
             MOVE     ZERO                TO   LINK-OUT-YMD
             CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                               LINK-IN-YMD6
                                               LINK-IN-YMD8
                                               LINK-OUT-RET
                                               LINK-OUT-YMD
             IF        LINK-OUT-RET  NOT =  ZERO
                       MOVE "1" TO MAIN-FLG
                       MOVE "R" TO EDIT-OPTION OF DSP-SIMEBI
                       MOVE "C" TO EDIT-CURSOR OF DSP-SIMEBI
                       IF    ERR-MSG-CD  =  0
                             MOVE 09  TO ERR-MSG-CD
                       END-IF
             ELSE
                       MOVE "D" TO EDIT-OPTION OF DSP-SIMEBI
                       MOVE " " TO EDIT-CURSOR OF DSP-SIMEBI
             END-IF
     END-IF.

*店舗コードチェック
     IF      DSP-TENCD      =   ZERO
             MOVE   "1"     TO  MAIN-FLG
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-TENCD
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-TENCD
             IF  ERR-MSG-CD     =   0
                 MOVE   20      TO  ERR-MSG-CD
             END-IF
     ELSE
             MOVE    2243             TO   TEN-F52
             MOVE    DSP-TENCD        TO   TEN-F011
             PERFORM                  TENREAD-SUB
     END-IF.

*伝票番号チェック
     IF      DSP-DENNO      =   ZERO
             MOVE   "1"     TO  MAIN-FLG
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-DENNO
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-DENNO
             IF  ERR-MSG-CD     =   0
                 MOVE   21      TO  ERR-MSG-CD
             END-IF
      ELSE
             PERFORM                  FILEREAD-SUB
      END-IF.
 HEAD1-CHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ヘッダ2入力　
*--------------------------------------------------------------*
 HEAD2-SUB                    SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "HEAD2"     TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
*
     EVALUATE DSP-FUNC
              WHEN  "F004"
              MOVE   SPACE        TO   DSP-HEAD01
              MOVE   SPACE        TO   DSP-HEAD02
              MOVE   SPACE        TO   DSP-BODY01
              MOVE   "D"          TO   EDIT-OPTION  OF  DSP-TENCD
              MOVE   SPACE        TO   EDIT-CURSOR  OF  DSP-TENCD
              MOVE   "0"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
                                       DSP-SGOKEI
              MOVE   SPACE        TO   DSP-KAKNIN
              GO                  TO   HEAD2-END
              WHEN "E000"
                   PERFORM   HEAD2-CHK-SUB
              WHEN "F005"
                   MOVE      "END"     TO   END-FLG
              WHEN "F009"
                   MOVE      "2"       TO   MAIN-FLG
                   MOVE      SPACE     TO   DSP-HEAD02
                   GO                  TO   HEAD2-SUB
     END-EVALUATE.
*
     IF       ERR-MSG-CD     =    ZERO
              MOVE      "3"  TO   MAIN-FLG
     END-IF.
*
 HEAD2-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ヘッダ2チェック
*--------------------------------------------------------------*
 HEAD2-CHK-SUB                SECTION.
*取引日付チェック
     IF      DSP-TORIDT     =   ZERO
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-TORIDT
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-TORIDT
             IF     ERR-MSG-CD   =     ZERO
                    MOVE         23    TO   ERR-MSG-CD
             END-IF
     ELSE
             MOVE  DSP-TORIDT(3:6)        TO   WK-CHKH-DATE
             MOVE     "3"                 TO   LINK-IN-KBN
             MOVE     WK-CHKH-DATE        TO   LINK-IN-YMD6
             MOVE     ZERO                TO   LINK-IN-YMD8
             MOVE     ZERO                TO   LINK-OUT-RET
             MOVE     ZERO                TO   LINK-OUT-YMD
             CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                               LINK-IN-YMD6
                                               LINK-IN-YMD8
                                               LINK-OUT-RET
                                               LINK-OUT-YMD
             IF        LINK-OUT-RET  NOT =  ZERO
                       MOVE "R" TO EDIT-OPTION OF DSP-TORIDT
                       MOVE "C" TO EDIT-CURSOR OF DSP-TORIDT
                       IF  ERR-MSG-CD     =   0
                           MOVE         14    TO   ERR-MSG-CD
                       END-IF
             ELSE
                       MOVE "D" TO EDIT-OPTION OF DSP-TORIDT
                       MOVE " " TO EDIT-CURSOR OF DSP-TORIDT
             END-IF
     END-IF.
*部門コードチェック
     IF      DSP-BUMON      =   SPACE
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-BUMON
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-BUMON
             IF     ERR-MSG-CD  =     ZERO
                    MOVE        15    TO   ERR-MSG-CD
             END-IF
     ELSE
             MOVE   "D"     TO  EDIT-OPTION  OF  DSP-BUMON
             MOVE   " "     TO  EDIT-CURSOR  OF  DSP-BUMON
     END-IF.
*項目コードチェック
*****IF      DSP-KOUMOK    =   SPACE
*            MOVE   "R"     TO  EDIT-OPTION  OF  DSP-KOUMOK
*            MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-KOUMOK
*            IF     ERR-MSG-CD  =     ZERO
*                   MOVE        16    TO   ERR-MSG-CD
*            END-IF
*    ELSE
*            MOVE   "D"     TO  EDIT-OPTION  OF  DSP-KOUMOK
*            MOVE   " "     TO  EDIT-CURSOR  OF  DSP-KOUMOK
*****END-IF.
*伝区チェック
     IF      DSP-DENKU   NOT =  "31" AND "21" AND "11"
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-DENKU
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-DENKU
             IF     ERR-MSG-CD  =     ZERO
                    MOVE        12    TO   ERR-MSG-CD
             END-IF
     ELSE
             MOVE   "D"     TO  EDIT-OPTION  OF  DSP-DENKU
             MOVE   " "     TO  EDIT-CURSOR  OF  DSP-DENKU
     END-IF.
*区分チェック
     IF      DSP-KBNK    NOT =  1 AND 9
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-KBNK
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-KBNK
             IF     ERR-MSG-CD  =     ZERO
                    MOVE        18    TO   ERR-MSG-CD
             END-IF
     ELSE
             MOVE   "D"     TO  EDIT-OPTION  OF  DSP-KBNK
             MOVE   " "     TO  EDIT-CURSOR  OF  DSP-KBNK
     END-IF.

 HEAD2-CHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ボディー入力                                  *
*--------------------------------------------------------------*
 BODY-SUB                    SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     IF       DSP-KUBUN  =   1
              INITIALIZE              DSP-BODY01
     END-IF.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "BODY"     TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
*
     EVALUATE DSP-FUNC
              WHEN  "F004"
              MOVE   SPACE        TO   DSP-HEAD01
              MOVE   SPACE        TO   DSP-HEAD02
              MOVE   SPACE        TO   DSP-BODY01
              MOVE   "D"          TO   EDIT-OPTION  OF  DSP-TENCD
              MOVE   SPACE        TO   EDIT-CURSOR  OF  DSP-TENCD
              MOVE   "0"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
                                       DSP-SGOKEI
              MOVE   SPACE        TO   DSP-KAKNIN
              GO                  TO   BODY-END
              WHEN "E000"
                   MOVE      ZERO      TO   WK-SGOKEI
                   MOVE      1         TO   GYO-CNT
                   PERFORM   KEISAN1-SUB    UNTIL  GYO-CNT = 8
                   MOVE      WK-SGOKEI TO   DSP-SGOKEI
              WHEN "F005"
                   MOVE      "END"     TO   END-FLG
              WHEN "F009"
                   MOVE      "3"       TO   MAIN-FLG
                   MOVE      SPACE     TO   DSP-BODY01
                   GO                  TO   BODY-SUB
     END-EVALUATE.
     IF       ERR-MSG-CD     =    ZERO
              MOVE      "4"  TO   MAIN-FLG
     END-IF.
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2.1   　　表計算1(登録・更新用）
*--------------------------------------------------------------*
 KEISAN1-SUB             SECTION.
*
*    合計初期化
     MOVE    ZERO            TO   WK-GOKEI.
     COMPUTE WK-GOKEI  =
                DSP-SURYO(GYO-CNT) * DSP-TANKA(GYO-CNT).
     MOVE    WK-GOKEI     TO  DSP-GOKEI(GYO-CNT).
     COMPUTE WK-SGOKEI = WK-SGOKEI + WK-GOKEI.

*    カウントアップ
     ADD      1              TO   GYO-CNT.

 KEISAN1-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.1     画面表示処理                                  *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE    "ALL"           TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     WRITE    DSP-FSE71151.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.2     エラーメッセージセット                        *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
* エラー メッセージ セット
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE       TO   DSP-MSG1
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)   TO   DSP-MSG1
         MOVE    ZERO                  TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.3     画面データの入力処理                          *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"         TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4     ファイル読込み処理                           *
*--------------------------------------------------------------*
 FILEREAD-SUB            SECTION.
*
*請求合計ファイル読込み
     MOVE       DSP-SIMEBI   TO  SEI-F01.
     MOVE       DSP-TENCD    TO  SEI-F02.
     MOVE       DSP-DENNO    TO  SEI-F03.
     PERFORM    SEIREAD-SUB.
*
 FILEREAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2   請求ファイルＲＥＡＤ                          *
*--------------------------------------------------------------*
 SEIREAD-SUB            SECTION.
     READ    JHJSEKF
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
*　登録時
     IF  DSP-KUBUN      =    1
*        すでに登録されていたらエラー
         IF  INVALID-FLG    =    ZERO
             IF  ERR-MSG-CD     =   0
                 MOVE   06      TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-DENNO
*            MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-DENNO
*　　　　未登録
         ELSE
             IF  ERR-MSG-CD     =   0
                 MOVE   ZERO    TO   ERR-MSG-CD
             END-IF
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-DENNO
         END-IF
*　修正・削除時
     ELSE
         IF  INVALID-FLG    =    1
             IF  ERR-MSG-CD     =   0
                 MOVE   02      TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-DENNO
*            MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-DENNO
         ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
             IF  ERR-MSG-CD     =   0
                 MOVE   ZERO    TO   ERR-MSG-CD
             END-IF
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-DENNO
             PERFORM        RECSET-SUB
     END-IF.
*
 TOKREAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2.1  項目セット                                   *
*--------------------------------------------------------------*
 RECSET-SUB             SECTION.
     MOVE    SEI-F02          TO   DSP-TENCD.
*
     MOVE    2243             TO   TEN-F52.
     MOVE    SEI-F02          TO   TEN-F011.
     PERFORM                  TENREAD-SUB.
*
*    ヘッダ２
     MOVE    SEI-F04          TO   DSP-TORIDT.
     MOVE    SEI-F05          TO   DSP-BUMON.
     MOVE    SEI-F06          TO   DSP-KOUMOK.
     MOVE    SEI-F07          TO   DSP-DENKU.
     MOVE    SEI-F08          TO   DSP-KBNK.

*　　ボディー
     MOVE    ZERO             TO       WK-SGOKEI.
     MOVE    1                TO       GYO-CNT.
     PERFORM KEISAN2-SUB      UNTIL    GYO-CNT   =   8.
     MOVE    WK-SGOKEI        TO       DSP-SGOKEI.
*
 RECSET-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2.1   　　表計算2（出力用）
*--------------------------------------------------------------*
 KEISAN2-SUB             SECTION.
*
*　　合計用ﾜｰｸ初期化
     MOVE    ZERO                 TO   WK-GOKEI.
     MOVE    SEI-F091(GYO-CNT)    TO   DSP-SURYO(GYO-CNT).
     MOVE    SEI-F092(GYO-CNT)    TO   DSP-TANKA(GYO-CNT).
     COMPUTE WK-GOKEI  = SEI-F091(GYO-CNT) * SEI-F092(GYO-CNT).
     COMPUTE WK-SGOKEI = WK-SGOKEI        + WK-GOKEI.
     MOVE    WK-GOKEI             TO   DSP-GOKEI(GYO-CNT).

*    カウントアップ
     ADD     1                    TO   GYO-CNT.

 KEISAN2-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.1   店舗マスタＲＥＡＤ                            *
*--------------------------------------------------------------*
 TENREAD-SUB            SECTION.
     READ    HTENMS
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG    =    1
         MOVE   "1"     TO   MAIN-FLG
             IF  ERR-MSG-CD     =   0
                 MOVE   08    TO   ERR-MSG-CD
             END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TENCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TENCD
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     ELSE
         MOVE   TEN-F02 TO   DSP-TENNM
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TENCD
         MOVE   " "     TO   EDIT-CURSOR  OF  DSP-TENCD
     END-IF.
*
 TENREAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF  ERR-MSG-CD  =  ZERO
         MOVE     "Y"       TO   DSP-KAKNIN
     END-IF.
     PERFORM       MSG-SEC.
     IF  DSP-KUBUN   =  3
         MOVE      PMSG04   TO   DSP-MSG2
     ELSE
             MOVE     PMSG01    TO   DSP-MSG2
     END-IF.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "TAIL"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE     ZERO           TO    ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              MOVE   SPACE        TO   DSP-HEAD01
              MOVE   SPACE        TO   DSP-HEAD02
              MOVE   SPACE        TO   DSP-BODY01
              MOVE   "D"          TO   EDIT-OPTION  OF  DSP-TENCD
              MOVE   SPACE        TO   EDIT-CURSOR  OF  DSP-TENCD
              MOVE   "0"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
                                       DSP-SGOKEI
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "F009"
              IF     DSP-KUBUN    =    3
                     MOVE  01     TO   ERR-MSG-CD
                     GO           TO   KAKUNIN-SUB
              END-IF
              MOVE   "2"          TO   MAIN-FLG
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "E000"
              IF  DSP-KAKNIN  NOT  =  "Y"
                  MOVE   05       TO   ERR-MSG-CD
              ELSE
                  MOVE   "5"      TO   MAIN-FLG
                  MOVE   SPACE    TO   DSP-KAKNIN
                  PERFORM              F-UPDATE-SEC
              END-IF
        WHEN  OTHER
              MOVE   01      TO   ERR-MSG-CD
     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5                                                     *
*--------------------------------------------------------------*
 F-UPDATE-SEC           SECTION.
*
     IF         DSP-KUBUN     =       1
        MOVE    SPACE         TO      SEI-REC
        INITIALIZE            SEI-REC
        MOVE    2243          TO      SEI-F01
        MOVE    DSP-DENNO     TO      SEI-F03
     END-IF.
*
*    ヘッダ２
     MOVE       DSP-SIMEBI    TO      SEI-F01.
     MOVE       DSP-TENCD     TO      SEI-F02.
     MOVE       DSP-DENNO     TO      SEI-F03.
     MOVE       DSP-TORIDT    TO      SEI-F04.
     MOVE       DSP-BUMON     TO      SEI-F05.
     MOVE       DSP-KOUMOK    TO      SEI-F06.
     MOVE       DSP-DENKU     TO      SEI-F07.
     MOVE       DSP-KBNK      TO      SEI-F08.
*    ボディー
     MOVE       ZERO          TO      WK-SGOKEI.
     MOVE       1             TO      GYO-CNT.
     PERFORM    BODY-UPDATE-SUB       UNTIL   GYO-CNT  =  8.
     MOVE       DSP-SGOKEI    TO      SEI-F10.
 F-UPDATE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2.1   　　ボディー更新                            *
*--------------------------------------------------------------*
 BODY-UPDATE-SUB             SECTION.
*
     MOVE    ZERO                 TO   WK-GOKEI.
     MOVE    DSP-SURYO(GYO-CNT)   TO   SEI-F091(GYO-CNT).
     MOVE    DSP-TANKA(GYO-CNT)   TO   SEI-F092(GYO-CNT).
     MOVE    DSP-GOKEI(GYO-CNT)   TO   SEI-F093(GYO-CNT).

*    カウントアップ
     ADD     1                    TO    GYO-CNT.

 BODY-UPDATE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5        ファイル削除                                 *
*--------------------------------------------------------------*
 FILDEL-SUB             SECTION.
     EVALUATE   DSP-KUBUN
        WHEN    1
                  WRITE      SEI-REC
        WHEN    2
                  REWRITE    SEI-REC
        WHEN    3
                  DELETE     JHJSEKF
     END-EVALUATE.
*
     MOVE    DSP-KUBUN  TO   WK-SYORI-KBN.
     MOVE   "D"         TO   EDIT-OPTION  OF  DSP-DENNO.
     MOVE   "D"         TO   EDIT-OPTION  OF  DSP-TENCD.
     MOVE    SPACE      TO   DSP-HEAD01.
     MOVE    SPACE      TO   DSP-HEAD02.
     MOVE    SPACE      TO   DSP-BODY01
                             DSP-KAKNIN.
     MOVE    ZERO       TO   DSP-SGOKEI.
     MOVE    WK-SYORI-KBN TO DSP-KUBUN.
     MOVE   "1"         TO   MAIN-FLG.
 FILDEL-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      JHJSEKF     HTENMS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
