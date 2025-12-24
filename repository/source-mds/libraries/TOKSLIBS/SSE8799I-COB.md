# SSE8799I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE8799I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求データ削除　　　　　　　　　　*
*    作成日／更新日　　　：　08/08/20                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求データの登録・修正・削除　　　*
*    処理概要　　　　　　：　を　行う　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE8799I.
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
     SELECT   SETGK873   ASSIGN    TO        DA-01-VI-SETGK873
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SEI-F01
                                                 SEI-F03
                                                 SEI-F14
                                                 SEI-F04
                                                 SEI-F05
                        FILE      STATUS    IS   SEI-STATUS.
*
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
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
     COPY     FSE87991   OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  請求合計ファイル  >>---*
 FD  SETGK873.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
     02  TOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
 01  WK-DSP-TORICD           PIC  9(08)  VALUE ZERO.
 01  WK-DSP-TENPCD           PIC  9(05)  VALUE ZERO.
 01  WK-DSP-TORINM           PIC  N(15)  VALUE SPACE.
 01  WK-DSP-TENPNM           PIC  N(15)  VALUE SPACE.
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
*
 01  WK-HATDT                PIC  X(08).
 01  WK-NOU-R    REDEFINES   WK-HATDT.
     02  WK-HATDT-H          PIC  9(08).
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
                           NC"_終了".
     02  PMSG03            PIC N(20) VALUE
                           NC"_終了　_再入力".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSE8799I".
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
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"コードが未登録です。".
     02  MSG-ERR3            PIC  N(28) VALUE
            NC"取引先名が未登録です。".
     02  MSG-ERR4            PIC  N(28) VALUE
            NC"店舗名が未登録です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"Ｙで入力して下さい。".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"既に登録済です。".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"区分が違います。".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"店舗コード未登録。".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"締切日が違います。".
     02  MSG-ERR10           PIC  N(28) VALUE
            NC"納品日が違います。".
     02  MSG-ERR11           PIC  N(28) VALUE
            NC"伝票_未入力".
     02  MSG-ERR12           PIC  N(28) VALUE
            NC"伝票区分エラー".
     02  MSG-ERR13           PIC  N(28) VALUE
            NC"発注日エラー".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS 13   TIMES.
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
                     PROCEDURE    SETGK873.
     MOVE     "SETGK873"      TO   ERR-FL-ID.
     MOVE     SEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTOKMS.
     MOVE     "HTOKMS"       TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTENMS.
     MOVE     "HTENMS"       TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 SSE8799I-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SSE8799I-END.
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
     OPEN    I-O             SETGK873.
     OPEN    INPUT           HTOKMS     HTENMS.
     MOVE    "FSE87991"       TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FSE87991.
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
         WHEN      "1"       PERFORM   HEAD-SUB
         WHEN      "2"       PERFORM   BODY-SUB
         WHEN      "3"       PERFORM   KAKUNIN-SUB
         WHEN      "4"       PERFORM   FILDEL-SUB
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
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "E000"
           IF    DSP-KUBUN  NOT =   1 AND 2 AND 3
                 MOVE   "0"   TO   MAIN-FLG
                 MOVE   "R"     TO
                                EDIT-OPTION  OF  DSP-KUBUN
                 MOVE   "C"     TO
                                EDIT-CURSOR  OF  DSP-KUBUN
                 MOVE    07     TO   ERR-MSG-CD
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
*      2.1       ヘッド部入力                                  *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "HEAD"     TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "F009"
           MOVE   "0"          TO   MAIN-FLG
           MOVE   SPACE        TO   DSP-HEAD01
           MOVE   SPACE        TO   DSP-BODY01
           MOVE   "D"          TO   EDIT-OPTION OF DSP-TORICD
           MOVE   "D"          TO   EDIT-OPTION OF DSP-TENPCD
           MOVE   "D"          TO   EDIT-OPTION OF DSP-NOUDT
           MOVE   "D"          TO   EDIT-OPTION OF DSP-DENNO
           MOVE   "D"          TO   EDIT-OPTION OF DSP-DENKU
           MOVE   "D"          TO   EDIT-OPTION OF DSP-KINGAK
           MOVE   "D"          TO   EDIT-OPTION OF DSP-SURYO
           MOVE   "D"          TO   EDIT-OPTION OF DSP-SIME01
           MOVE   "D"          TO   EDIT-OPTION OF DSP-SIME02
           MOVE   "D"          TO   EDIT-OPTION OF DSP-SIME03
           MOVE   "D"          TO   EDIT-OPTION OF DSP-BUNRUI
           MOVE   "D"          TO   EDIT-OPTION OF DSP-HATDT
       WHEN
        "E000"
           PERFORM  HEAD-CHK-SUB
           IF     ERR-MSG-CD   =    ZERO
                  IF  DSP-KUBUN    =    3
                      MOVE   "3"   TO   MAIN-FLG
                  ELSE
                      MOVE   "2"   TO   MAIN-FLG
                  END-IF
                  MOVE   "D"     TO
                          EDIT-OPTION  OF  DSP-TORICD
                          EDIT-OPTION  OF  DSP-DENNO
                  MOVE   SPACE   TO
                          EDIT-CURSOR  OF  DSP-TORICD
                          EDIT-CURSOR  OF  DSP-DENNO
           END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ボディー入力                                  *
*--------------------------------------------------------------*
 HEAD-CHK-SUB                SECTION.
*取引先ＣＤチェック
     MOVE        DSP-TORICD   TO  TOK-F01.
     PERFORM     TOKREAD-SUB
*****IF          DSP-DENNO      =    ZERO
*           IF   ERR-MSG-CD     =    ZERO
*                MOVE   11      TO   ERR-MSG-CD
*           END-IF
*                  MOVE   "R"     TO
*                         EDIT-OPTION  OF  DSP-DENNO
*                  MOVE   "C"     TO
*                         EDIT-CURSOR  OF  DSP-DENNO
*****END-IF.
*店舗コードチェック
     MOVE    DSP-TORICD       TO   TEN-F52.
     MOVE    DSP-TENPCD       TO   TEN-F011.
     PERFORM            TENREAD-SUB.
*
     IF      INVALID-FLG      =    1
             IF  ERR-MSG-CD NOT = ZERO
                 MOVE   08      TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TENPCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TENPCD
     ELSE
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TENPCD
             MOVE    SPACE  TO   EDIT-CURSOR  OF  DSP-TENPCD
     END-IF.
*納品日日付チェック
     IF      DSP-NOUDT      =    ZERO
     OR      DSP-NOUDT  NOT  NUMERIC
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-NOUDT
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-NOUDT
             IF     ERR-MSG-CD   =     ZERO
                    MOVE         10    TO   ERR-MSG-CD
             END-IF
     ELSE
             MOVE     "2"                 TO   LINK-IN-KBN
             MOVE     ZERO                TO   LINK-IN-YMD6
             MOVE     DSP-NOUDT           TO   LINK-IN-YMD8
             MOVE     ZERO                TO   LINK-OUT-RET
             MOVE     ZERO                TO   LINK-OUT-YMD
             CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                               LINK-IN-YMD6
                                               LINK-IN-YMD8
                                               LINK-OUT-RET
                                               LINK-OUT-YMD
             IF        LINK-OUT-RET  NOT =  ZERO
                       MOVE "R" TO EDIT-OPTION OF DSP-NOUDT
                       MOVE "C" TO EDIT-CURSOR OF DSP-NOUDT
                       MOVE         09    TO   ERR-MSG-CD
             ELSE
                       MOVE DSP-NOUDT(3:2)    TO   DSP-NOU01
                       MOVE DSP-NOUDT(5:2)    TO   DSP-NOU02
                       MOVE DSP-NOUDT(7:2)    TO   DSP-NOU03
                       MOVE "D" TO EDIT-OPTION OF DSP-NOUDT
                       MOVE "C" TO EDIT-CURSOR OF DSP-NOUDT
             END-IF
     END-IF.
*伝票番号チェック
     IF      DSP-DENNO  =  SPACE
     OR      DSP-DENNO(1:1) = SPACE
     OR      DSP-DENNO(2:1) = SPACE
     OR      DSP-DENNO(3:1) = SPACE
     OR      DSP-DENNO(4:1) = SPACE
     OR      DSP-DENNO(5:1) = SPACE
     OR      DSP-DENNO(6:1) = SPACE
     OR      DSP-DENNO(7:1) = SPACE
     OR      DSP-DENNO(8:1) = SPACE
     OR      DSP-DENNO(9:1) = SPACE
             IF  ERR-MSG-CD NOT = ZERO
                 MOVE   11      TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-DENNO
     ELSE
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE    SPACE  TO   EDIT-CURSOR  OF  DSP-DENNO
     END-IF.
*ファイル存在チェック
     PERFORM       FILEREAD-SUB.
 HEAD-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.        ボディー入力                                  *
*--------------------------------------------------------------*
 BODY-SUB                    SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "BODY"     TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
*
     EVALUATE DSP-FUNC
              WHEN "E000"
                   PERFORM   BODY-CHK-SUB
              WHEN "F005"
                   MOVE      "END"     TO   END-FLG
              WHEN "F009"
                   MOVE   "1"   TO   MAIN-FLG
                   MOVE   SPACE TO   DSP-BODY01
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-DENKU
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-KINGAK
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-SURYO
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-SIME01
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-SIME02
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-SIME03
                   MOVE   "D"   TO   EDIT-OPTION OF DSP-BUNRUI
                   GO           TO   BODY-END
     END-EVALUATE.
*
     IF       ERR-MSG-CD     =    ZERO
              MOVE      "3"  TO   MAIN-FLG
     END-IF.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.        ボディー入力                                  *
*--------------------------------------------------------------*
 BODY-CHK-SUB                SECTION.
*
*伝票区分チェック
     MOVE   ZERO             TO   ERR-MSG-CD.
     MOVE  "D"    TO EDIT-OPTION  OF    DSP-DENKU.
     MOVE  SPACE  TO EDIT-CURSOR  OF    DSP-DENKU.
*****IF      DSP-TORICD       =    173
         EVALUATE DSP-DENKU
             WHEN 40
                  MOVE  NC"売上"           TO      DSP-URIAGE
             WHEN 41
                  MOVE  NC"売上返品"       TO      DSP-URIAGE
             WHEN 42
                  MOVE  NC"売上値引"       TO      DSP-URIAGE
             WHEN 44
                  MOVE  NC"売上割戻"       TO      DSP-URIAGE
             WHEN 45
                  MOVE  NC"売上値増"       TO      DSP-URIAGE
             WHEN OTHER
                  MOVE  "R" TO  EDIT-OPTION  OF    DSP-DENKU
                  MOVE  "C" TO  EDIT-CURSOR  OF    DSP-DENKU
                  MOVE   12 TO   ERR-MSG-CD
                  MOVE  ALL NC"＊"         TO      DSP-URIAGE
         END-EVALUATE.
*****END-IF.
*
*日付チェック
*****IF      DSP-SIME01     =    ZERO      OR
     IF      DSP-SIME02     =    ZERO      OR
             DSP-SIME03     =    ZERO
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-SIME01
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-SIME02
             MOVE   "R"     TO  EDIT-OPTION  OF  DSP-SIME03
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-SIME01
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-SIME02
             MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-SIME03
             IF     ERR-MSG-CD   =     ZERO
                    MOVE         09    TO   ERR-MSG-CD
             END-IF
     ELSE
             MOVE  DSP-SIME01 TO WK-CHKH-YY
             MOVE  DSP-SIME02 TO WK-CHKH-MM
             MOVE  DSP-SIME03 TO WK-CHKH-DD
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
                       MOVE "R" TO EDIT-OPTION OF DSP-SIME01
                       MOVE "R" TO EDIT-OPTION OF DSP-SIME02
                       MOVE "R" TO EDIT-OPTION OF DSP-SIME03
                       MOVE "C" TO EDIT-CURSOR OF DSP-SIME01
                       MOVE         09    TO   ERR-MSG-CD
             ELSE
                       MOVE "D" TO EDIT-OPTION OF DSP-SIME01
                       MOVE "D" TO EDIT-OPTION OF DSP-SIME02
                       MOVE "D" TO EDIT-OPTION OF DSP-SIME03
                       MOVE SPACE TO EDIT-CURSOR OF DSP-SIME01
             END-IF
     END-IF.
*納品日日付チェック
     IF      DSP-HATDT      =    ZERO
     OR      DSP-HATDT  NOT  NUMERIC
*************MOVE   "R"     TO  EDIT-OPTION  OF  DSP-HATDT
*            MOVE   "C"     TO  EDIT-CURSOR  OF  DSP-HATDT
*            IF     ERR-MSG-CD   =     ZERO
*                   MOVE         13    TO   ERR-MSG-CD
*************END-IF
             MOVE DSP-NOUDT TO  DSP-HATDT
     ELSE
             MOVE     "2"                 TO   LINK-IN-KBN
             MOVE     ZERO                TO   LINK-IN-YMD6
             MOVE     DSP-HATDT           TO   LINK-IN-YMD8
             MOVE     ZERO                TO   LINK-OUT-RET
             MOVE     ZERO                TO   LINK-OUT-YMD
             CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                               LINK-IN-YMD6
                                               LINK-IN-YMD8
                                               LINK-OUT-RET
                                               LINK-OUT-YMD
             IF        LINK-OUT-RET  NOT =  ZERO
                       MOVE "R"   TO EDIT-OPTION OF DSP-HATDT
                       MOVE "C"   TO EDIT-CURSOR OF DSP-HATDT
                       MOVE  13   TO ERR-MSG-CD
             ELSE
                       MOVE "D"   TO EDIT-OPTION OF DSP-HATDT
                       MOVE SPACE TO EDIT-CURSOR OF DSP-HATDT
             END-IF
     END-IF.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.1     画面表示処理                                  *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE    "ALL"           TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     WRITE    DSP-FSE87991.
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
*取引先マスタ読込み
*****MOVE       DSP-TORICD   TO  TOK-F01.
*****PERFORM    TOKREAD-SUB.
*
     IF  ERR-MSG-CD  NOT = ZERO
                GO      TO   FILEREAD-END
     END-IF.
*
*請求合計ファイル読込み
     MOVE       DSP-TORICD   TO  SEI-F01.
     MOVE       DSP-TENPCD   TO  SEI-F03.
     MOVE       DSP-NOUDT    TO  SEI-F14.
     MOVE       DSP-NOU01    TO  SEI-F04(1:2).
     MOVE       DSP-NOU02    TO  SEI-F04(3:2).
     MOVE       DSP-NOU03    TO  SEI-F04(5:2).
     MOVE       DSP-DENNO    TO  SEI-F05.
     PERFORM    SEIREAD-SUB.
*
 FILEREAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.1   取引先マスタＲＥＡＤ                          *
*--------------------------------------------------------------*
 TOKREAD-SUB            SECTION.
     READ    HTOKMS
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG    =    1
         MOVE   SPACE   TO   DSP-TORINM
                 MOVE   "R"     TO
                                EDIT-OPTION  OF  DSP-TORICD
                 MOVE   "C"     TO
                                EDIT-CURSOR  OF  DSP-TORICD
                 MOVE    03     TO   ERR-MSG-CD
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         MOVE   TOK-F02 TO   DSP-TORINM
         MOVE   ZERO    TO   ERR-MSG-CD
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TORICD
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TORICD
     END-IF.
*
 TOKREAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2   請求合計ファイルＲＥＡＤ                      *
*--------------------------------------------------------------*
 SEIREAD-SUB            SECTION.
     READ    SETGK873
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  DSP-KUBUN      =    1
         IF  INVALID-FLG    =    ZERO
             MOVE   06      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TORICD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TENPCD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-NOUDT
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TORICD
         ELSE
             MOVE   ZERO    TO   ERR-MSG-CD
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TORICD
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TENPCD
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-NOUDT
             MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TORICD
         END-IF
     ELSE
         IF  INVALID-FLG    =    1
             MOVE   02      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TORICD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TENPCD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-NOUDT
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TORICD
         ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
             MOVE   ZERO    TO   ERR-MSG-CD
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TORICD
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-DENNO
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TENPCD
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-NOUDT
             MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TORICD
             PERFORM        RECSET-SUB
     END-IF.
*
 TOKREAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4.2.1  項目セット                                   *
*--------------------------------------------------------------*
 RECSET-SUB             SECTION.
     MOVE    SEI-F03          TO   DSP-TENPCD.
*
     MOVE    SEI-F01          TO   TEN-F52.
     MOVE    SEI-F03          TO   TEN-F011.
     PERFORM            TENREAD-SUB.
*
     MOVE    SEI-F07          TO   DSP-DENKU.
     MOVE    SEI-F06          TO   DSP-KINGAK.
     MOVE    SEI-F09          TO   DSP-SURYO.
     MOVE    SEI-F02          TO   WK-SIME.
     MOVE    WK-SIME01        TO   DSP-SIME01.
     MOVE    WK-SIME02        TO   DSP-SIME02
     MOVE    WK-SIME03        TO   DSP-SIME03.
     MOVE    SEI-F10          TO   WK-NOUHIN.
     MOVE    WK-NOU01         TO   DSP-NOU01.
     MOVE    WK-NOU02         TO   DSP-NOU02.
     MOVE    WK-NOU03         TO   DSP-NOU03.
****ADD 2008/08/20)NAV 後閑 BGN
     MOVE    SEI-F12          TO   DSP-BUNRUI.
****ADD 2008/08/20)NAV 後閑 END
*****IF      SEI-F01          =    173
         EVALUATE SEI-F07
             WHEN 40
                  MOVE  NC"売上"           TO      DSP-URIAGE
             WHEN 41
                  MOVE  NC"売上返品"       TO      DSP-URIAGE
             WHEN 42
                  MOVE  NC"売上値引"       TO      DSP-URIAGE
             WHEN 44
                  MOVE  NC"売上割戻"       TO      DSP-URIAGE
             WHEN 45
                  MOVE  NC"売上値増"       TO      DSP-URIAGE
             WHEN OTHER
                  MOVE  ALL NC"＊"         TO      DSP-URIAGE
         END-EVALUATE.
*****END-IF.
     MOVE    SEI-F13          TO   WK-HATDT.
     MOVE    WK-HATDT-H       TO   DSP-HATDT.
*
 RECSET-END.
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
         MOVE   SPACE   TO   DSP-TENPNM
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         MOVE   TEN-F02 TO   DSP-TENPNM
*********MOVE   ZERO    TO   ERR-MSG-CD
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TENPCD
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
     MOVE     PMSG01    TO   DSP-MSG2
     PERFORM       DSP-WRITE-SUB.
     MOVE    "TAIL"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE     ZERO           TO    ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              MOVE   ZERO         TO   DSP-TORICD
              MOVE   SPACE        TO   DSP-TORINM
              MOVE   SPACE        TO   DSP-DENNO
              MOVE   SPACE        TO   DSP-BODY01
              MOVE   "D"          TO   EDIT-OPTION  OF  DSP-TENPCD
              MOVE   SPACE        TO   EDIT-CURSOR  OF  DSP-TENPCD
              MOVE   "1"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "F009"
              MOVE   "2"          TO   MAIN-FLG
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "E000"
              IF  DSP-KAKNIN  NOT  =  "Y"
                  MOVE   05      TO   ERR-MSG-CD
              ELSE
                  MOVE   "4"     TO   MAIN-FLG
                  MOVE   SPACE   TO   DSP-KAKNIN
                  PERFORM             F-UPDATE-SEC
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
     IF         DSP-KUBUN   =    1
        MOVE   SPACE     TO   SEI-REC
        INITIALIZE            SEI-REC
        MOVE    DSP-TORICD    TO      SEI-F01
        MOVE    DSP-DENNO     TO      SEI-F05
     END-IF.
*
     MOVE       DSP-DENKU     TO      SEI-F07.
     MOVE       DSP-TENPCD    TO      SEI-F03.
     MOVE       DSP-KINGAK    TO      SEI-F06.
     MOVE       DSP-SURYO     TO      SEI-F09.
     MOVE       DSP-SIME01    TO      SEI-F02(1:2).
     MOVE       DSP-SIME02    TO      SEI-F02(3:2).
     MOVE       DSP-SIME03    TO      SEI-F02(5:2).
     MOVE       DSP-NOU01     TO      SEI-F10(1:2).
     MOVE       DSP-NOU02     TO      SEI-F10(3:2).
     MOVE       DSP-NOU03     TO      SEI-F10(5:2).
     MOVE       DSP-NOU01     TO      SEI-F04(1:2).
     MOVE       DSP-NOU02     TO      SEI-F04(3:2).
     MOVE       DSP-NOU03     TO      SEI-F04(5:2).
****ADD 2008/08/20)NAV 後閑 BGN
     MOVE       DSP-BUNRUI    TO      SEI-F12.
****ADD 2008/08/20)NAV 後閑 END
     MOVE       DSP-NOUDT     TO      SEI-F14.
     MOVE       DSP-HATDT     TO      SEI-F13.
*
 F-UPDATE-EXIT.
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
                  DELETE     SETGK873
     END-EVALUATE.
*
     MOVE    DSP-KUBUN  TO   WK-SYORI-KBN.
     MOVE   "D"         TO   EDIT-OPTION  OF  DSP-TORICD.
     MOVE   "D"         TO   EDIT-OPTION  OF  DSP-DENNO.
     MOVE   "D"         TO   EDIT-OPTION  OF  DSP-TENPCD.
     MOVE   "D"         TO   EDIT-OPTION  OF  DSP-NOUDT.
     MOVE   DSP-TORICD  TO   WK-DSP-TORICD.
     MOVE   DSP-TENPCD  TO   WK-DSP-TENPCD.
     MOVE   DSP-TORINM  TO   WK-DSP-TORINM.
     MOVE   DSP-TENPNM  TO   WK-DSP-TENPNM.
     MOVE    ZERO       TO   DSP-NOUDT.
     MOVE    SPACE      TO   DSP-DENNO.
     MOVE    SPACE      TO   DSP-HEAD01.
     MOVE    SPACE      TO   DSP-BODY01.
     MOVE    WK-SYORI-KBN TO DSP-KUBUN.
     MOVE   WK-DSP-TORICD  TO   DSP-TORICD.
     MOVE   WK-DSP-TENPCD  TO   DSP-TENPCD.
     MOVE   WK-DSP-TORINM  TO   DSP-TORINM.
     MOVE   WK-DSP-TENPNM  TO   DSP-TENPNM.
     MOVE   "1"         TO   MAIN-FLG.
 FILDEL-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      SETGK873     HTOKMS     HTENMS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
