****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　請求データ保守　　　　　　　　　　*
*    モジュール名　　　　：　請求データ保守　　　　　　　　　　*
*    作成日／更新日　　　：　2005/12/27                        *
*    作成者／更新者　　　：　ＮＡＶ松野　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求データの修正を行う。　　　　  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSS0010I.
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
                        DESTINATION-1       IS   DSP-WSNO
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  請求合計ファイル  >>---*
     SELECT   SETGKFA   ASSIGN    TO        DA-01-VI-SETGKFA1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SEI-F01
                                                 SEI-F05
                        FILE      STATUS    IS   SEI-STATUS.
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
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
 01  DSP-REC            PIC  X(2000).
     COPY     FSS00101  OF        XMDLIB.
*---<<  請求合計ファイル  >>---*
 FD  SETGKFA.
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
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-WSNO            PIC  X(08).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  SEI-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  SEI-FLG             PIC  9(01)  VALUE ZERO.
****  処理スイッチ  ****
 01  WK-AREA.
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
****  伝票_右詰用エリア      ****
 01  WK-HDENNO.
     03  WK-HDEN             PIC  X(01)   OCCURS   9.
****  システム日付  ****
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
     03  SYS-DATE2                PIC  9(08).
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
*
 01  SAV-KEY.
     03  SAV-BKEY.
       05  SAV-BKEY-TORICD     PIC  9(08)   VALUE  ZERO.
       05  SAV-BKEY-HDENNO     PIC  X(09)   VALUE  SPACE.
     03  SAV-NKEY.
       05  SAV-NKEY-TORICD     PIC  9(08)   VALUE  ZERO.
       05  SAV-NKEY-HDENNO     PIC  X(09)   VALUE  SPACE.
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(07) VALUE
             NC"_取消　_終了".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG03            PIC N(11) VALUE
             NC"_取消　_終了　_戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSS0010I".
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
     02  MSG-ERR1            PIC  N(20)  VALUE
             NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"仕入先マスタに存在しません".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"入力項目に間違いがあります".
     02  MSG-ERR4            PIC  N(20) VALUE
            NC"Ｙ，Ｈ，Ｂのいずれかで入力して下さい".
     02  MSG-ERR5            PIC  N(20) VALUE
            NC"ＹかＨで入力して下さい".
     02  MSG-ERR6            PIC  N(20) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"該当データがありません".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"前頁がありません".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"次頁がありません".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"自動発注区分に誤りがあります。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"定期区分に誤りがあります。".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"季節区分を入力して下さい。".
     02  MSG-ERR13           PIC  N(20)  VALUE
             NC"季節区分に誤りがあります。".
     02  MSG-ERR14           PIC  N(20)  VALUE
             NC"廃盤区分に誤りがあります。".
     02  MSG-ERR15           PIC  N(20)  VALUE
             NC"仕入先マスタに未登録です。".
     02  MSG-ERR16           PIC  N(20)  VALUE
             NC"締日論理エラー".
     02  MSG-ERR17           PIC  N(20)  VALUE
             NC"納品日論理エラー".
     02  MSG-ERR18           PIC  N(20)  VALUE
             NC"伝区に誤りがあります。".
     02  MSG-ERR19           PIC  N(20)  VALUE
             NC"取引先コードを入力してください。".
     02  MSG-ERR20           PIC  N(20)  VALUE
             NC"取引先マスタに登録されていません。".
     02  MSG-ERR21           PIC  N(20)  VALUE
             NC"位置付伝票_を入力してください。".
     02  MSG-ERR22           PIC  N(20)  VALUE
             NC"取引先ＣＤは変更できません。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 22  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*==========================================================*
 LINKAGE                     SECTION.
*==========================================================*
 01  LINK-IN-TORICD        PIC  9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE           DIVISION   USING    LINK-IN-TORICD.
**
 DECLARATIVES.
 FILEERR-DSP            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEI            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SETGKFA.
     MOVE     "SETGKFA"        TO   ERR-FL-ID.
     MOVE      SEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-TOK            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE     "HTOKMS "        TO   ERR-FL-ID.
     MOVE      TOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-TEN            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE     "HTENMS "        TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SSS0010I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SSS0010I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF      SETGKFA.
     OPEN     INPUT     HTOKMS    HTENMS.
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
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     SPACE          TO   FSS00101.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
*パラメタ受取判別　　
     IF     LINK-IN-TORICD  NOT = SPACE
            MOVE   LINK-IN-TORICD  TO   TORICD
            MOVE   TORICD          TO   TOK-F01
            PERFORM     TOK-READ-SUB
            IF   INVALID-FLG = 1
                 DISPLAY NC"取引先が登録されていません。"
                                           UPON CONS
                 DISPLAY "LINK-IN-TORICD =" LINK-IN-TORICD
                                           UPON CONS
                 STOP   RUN
            ELSE
                 MOVE   TOK-F02  TO   TORINM
                 MOVE  "M"   TO  EDIT-OPTION  OF  HDENNO
                 MOVE  "C"   TO  EDIT-CURSOR  OF  HDENNO
            END-IF
     END-IF.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   HEAD-SUB
         WHEN      "2"  PERFORM   BODY-SUB
         WHEN      "3"  PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*==========================================================*
*      2.1       ヘッド部入力              MAIN-FLG=1      *
*==========================================================*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "HEAD"          TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FSS00101
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
                        IF LINK-IN-TORICD  NOT = SPACE
                           MOVE LINK-IN-TORICD  TO   TORICD
                           MOVE TORICD          TO   TOK-F01
                           PERFORM     TOK-READ-SUB
                           IF   INVALID-FLG = 1
                                DISPLAY
                                NC"取引先が登録されていません。"
                                                         UPON CONS
                                STOP   RUN
                           ELSE
                                MOVE  TOK-F02    TO     TORINM
                                MOVE "M" TO EDIT-OPTION OF HDENNO
                                MOVE "C" TO EDIT-CURSOR OF HDENNO
                           END-IF
                        END-IF
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "E000"
                        PERFORM   BODYDEL-SUB
                        PERFORM   SETGK-DSP-SUB
                        IF   ERR-MSG-CD     =    ZERO
                             MOVE     "2"   TO   MAIN-FLG
                             MOVE "M"  TO EDIT-OPTION OF TORICD
                             MOVE " "  TO EDIT-CURSOR OF TORICD
                             MOVE "M"  TO EDIT-OPTION OF HDENNO
                        END-IF
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*==========================================================*
*      2.1.2     明細セット処理                            *
*==========================================================*
 SETGK-DSP-SUB       SECTION.
*
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*伝票_右詰処理
     IF  HDENNO    NOT =     SPACE
         MOVE      HDENNO    TO   WK-HDENNO
         PERFORM   UNTIL     WK-HDEN(9) NOT = SPACE
             PERFORM VARYING IXA FROM  8 BY -1 UNTIL IXA = 0
                   MOVE      WK-HDEN(IXA)    TO  WK-HDEN(IXA + 1)
             END-PERFORM
             MOVE      ZERO      TO  WK-HDEN(1)
         END-PERFORM
         MOVE      WK-HDENNO      TO   HDENNO
     END-IF.
*ヘッダチェック
*    取引先コードチェック
***　未入力チェック
     IF       TORICD    =    SPACE
              MOVE   19   TO  ERR-MSG-CD
              MOVE  "R"   TO  EDIT-OPTION  OF  TORICD
              MOVE  "C"   TO  EDIT-CURSOR  OF  TORICD
              GO     TO   SETGK-DSP-EXIT
     ELSE
***　取引先マスタ登録チェック
              MOVE TORICD TO  TOK-F01
              PERFORM     TOK-READ-SUB
              IF   INVALID-FLG = 1
                   MOVE  SPACE TO  TORINM
                   MOVE   20   TO  ERR-MSG-CD
                   MOVE  "R"   TO  EDIT-OPTION  OF  TORICD
                   MOVE  "C"   TO  EDIT-CURSOR  OF  TORICD
                   GO     TO   SETGK-DSP-EXIT
              ELSE
                   MOVE   TOK-F02 TO   TORINM
                   MOVE  "M"   TO  EDIT-OPTION  OF  TORICD
                   MOVE  " "   TO  EDIT-CURSOR  OF  TORICD
              END-IF
     END-IF.
*    位置付伝票_チェック
     IF       HDENNO    =    SPACE
              MOVE   21   TO  ERR-MSG-CD
              MOVE  "R"   TO  EDIT-OPTION  OF  HDENNO
              MOVE  "C"   TO  EDIT-CURSOR  OF  HDENNO
              GO     TO   SETGK-DSP-EXIT
     END-IF.
*取引先ＣＤ変更チェック
     IF       LINK-IN-TORICD NOT = SPACE
    AND       LINK-IN-TORICD NOT = TORICD
              MOVE   22                TO   ERR-MSG-CD
              MOVE   LINK-IN-TORICD    TO   TORICD
              MOVE   TORICD            TO   TOK-F01
              PERFORM     TOK-READ-SUB
              IF   INVALID-FLG = 1
                   DISPLAY NC"取引先が登録されていません。"
                                                    UPON CONS
                   STOP   RUN
              ELSE
                   MOVE   TOK-F02 TO   TORINM
                   MOVE "M" TO  EDIT-OPTION  OF  HDENNO
                   MOVE "C" TO  EDIT-CURSOR  OF  HDENNO
              END-IF
              GO      TO     SETGK-DSP-EXIT
     END-IF.
*データ読込　　
     PERFORM  SETGK-START-SUB.
     IF       SEI-FLG   =    ZERO
              PERFORM   SETGK-READ-SUB
              IF   SEI-FLG   =    1
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      7    TO   ERR-MSG-CD
                   END-IF
                   IF     LINK-IN-TORICD = SPACE
                          MOVE "R"  TO   EDIT-OPTION  OF  TORICD
                          MOVE "C"  TO   EDIT-CURSOR  OF  TORICD
                          MOVE "R"  TO   EDIT-OPTION  OF  HDENNO
                   ELSE
                          MOVE "R"  TO   EDIT-OPTION  OF  HDENNO
                          MOVE "C"  TO   EDIT-CURSOR  OF  HDENNO
                   END-IF
                   GO      TO     SETGK-DSP-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      7    TO   ERR-MSG-CD
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  TORICD
              MOVE   "C"     TO   EDIT-CURSOR  OF  TORICD
              MOVE   "R"     TO   EDIT-OPTION  OF  HDENNO
              GO      TO     SETGK-DSP-EXIT
     END-IF.
*明細セット
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    8 )  OR
                      ( SEI-FLG   =    1 )
*        伝票_
         MOVE      SEI-F05        TO   DENNO (IXA)
*        締日　　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F02            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   SIMEBI(IXA)
*        納品日　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F10            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   NOUDAY(IXA)
*        伝区　　
         MOVE      SEI-F07        TO   DENK  (IXA)
*        数量
         MOVE      SEI-F09        TO   SURYO (IXA)
*        金額　　
         MOVE      SEI-F06        TO   KINGAK(IXA)
*        店舗　　
         MOVE      SEI-F03        TO   TENCD (IXA) TEN-F011
         MOVE      TORICD         TO   TEN-F52
         PERFORM   TEN-READ-SUB
         IF        INVALID-FLG    =    1
                   MOVE   ALL NC"＊"   TO   TENNM(IXA)
         ELSE
                   MOVE   TEN-F02      TO   TENNM(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE SEI-F01        TO   SAV-BKEY-TORICD
              MOVE SEI-F05        TO   SAV-BKEY-HDENNO
         END-IF
         IF        IXA  =    8
              MOVE SEI-F01        TO   SAV-NKEY-TORICD
              MOVE SEI-F05        TO   SAV-NKEY-HDENNO
         END-IF
*
         PERFORM   SETGK-READ-SUB
     END-PERFORM.
*
 SETGK-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   請求合計ファイルＳＴＡＲＴ                *
*----------------------------------------------------------*
 SETGK-START-SUB        SECTION.
*
     MOVE    ZERO            TO   SEI-FLG.
     MOVE    SPACE           TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE    TORICD          TO   SEI-F01.
     MOVE    HDENNO          TO   SEI-F05.
     START   SETGKFA    KEY  >=   SEI-F01   SEI-F05
       INVALID      KEY
             MOVE       1         TO   SEI-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   SEI-FLG
     END-START.
 SETGK-START-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   請求合計ファイルＲＥＡＤ                  *
*----------------------------------------------------------*
 SETGK-READ-SUB        SECTION.
*
     READ    SETGKFA   NEXT
         AT END
             MOVE      1     TO   SEI-FLG
         NOT AT END
             IF   SEI-F01  NOT =  TORICD
                  MOVE  1    TO   SEI-FLG
                  GO   TO    SETGK-READ-END
             END-IF
             MOVE      ZERO  TO   SEI-FLG
     END-READ.
*
 SETGK-READ-END.
     EXIT.
*==========================================================*
*      2.2       ボディ部入力              MAIN-FLG=2      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FSS00101
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
                        IF LINK-IN-TORICD  NOT = SPACE
                           MOVE LINK-IN-TORICD  TO   TORICD
                           MOVE TORICD          TO   TOK-F01
                           PERFORM     TOK-READ-SUB
                           IF   INVALID-FLG = 1
                        DISPLAY NC"取引先が登録されていません。"
                                                         UPON CONS
                           STOP   RUN
                           ELSE
                           MOVE   TOK-F02 TO   TORINM
                           MOVE "M" TO  EDIT-OPTION  OF  HDENNO
                           MOVE "C" TO  EDIT-CURSOR  OF  HDENNO
                           END-IF
                        END-IF
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        PERFORM             EDIT-SET-BODY
                        MOVE    "1"    TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   SETGK-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
                        IF  DENNO(1) = SPACE
                            MOVE ZERO         TO   SEI-FLG
                            MOVE SPACE        TO   SEI-REC
                            INITIALIZE             SEI-REC
                            MOVE TORICD       TO   SEI-F01
                            MOVE ZERO         TO   SEI-F05
                            START SETGKFA KEY > SEI-F01
                                                SEI-F05
                            END-START
                            PERFORM   SETGK-READ-SUB
                            MOVE SPACE TO SAV-NKEY
                            PERFORM  BODYDEL-SUB
                            PERFORM VARYING IXA FROM 1 BY 1
                            UNTIL ( IXA     >   8 )  OR
                                  ( SEI-FLG =   1 )
*        伝票_
         MOVE      SEI-F05        TO   DENNO (IXA)
*        締日　　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F02            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   SIMEBI(IXA)
*        納品日　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F10            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   NOUDAY(IXA)
*        伝区　　
         MOVE      SEI-F07        TO   DENK  (IXA)
*        数量
         MOVE      SEI-F09        TO   SURYO (IXA)
*        金額　　
         MOVE      SEI-F06        TO   KINGAK(IXA)
*        店舗　　
         MOVE      SEI-F03        TO   TENCD (IXA) TEN-F011
         MOVE      TORICD         TO   TEN-F52
         PERFORM   TEN-READ-SUB
         IF        INVALID-FLG    =    1
                   MOVE   ALL NC"＊"   TO   TENNM(IXA)
         ELSE
                   MOVE   TEN-F02      TO   TENNM(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE SEI-F01        TO   SAV-BKEY-TORICD
              MOVE SEI-F05        TO   SAV-BKEY-HDENNO
         END-IF
         IF        IXA  =    8
              MOVE SEI-F01        TO   SAV-NKEY-TORICD
              MOVE SEI-F05        TO   SAV-NKEY-HDENNO
         END-IF
*
*
         PERFORM   SETGK-READ-SUB
     END-PERFORM
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   SETGK-UPD-SUB
                            PERFORM   NEXT-PAGE-SUB
                        END-IF
         WHEN   "E000"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "3"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK-SUB            SECTION.
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL     IXA    >   8
         IF   DENNO (IXA) =  LOW-VALUE
              MOVE   SPACE      TO   MAS001(IXA)
         END-IF
***      締日チェック
         IF   DENNO (IXA) NOT  =  SPACE
              MOVE  "3"                TO   LINK-IN-KBN
              MOVE  SIMEBI(IXA)        TO   LINK-IN-YMD6
              MOVE  ZERO               TO   LINK-IN-YMD8
              MOVE  ZERO               TO   LINK-OUT-RET
              MOVE  ZERO               TO   LINK-OUT-YMD
              CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
              IF    LINK-OUT-RET    NOT =   ZERO
                    IF   ERR-MSG-CD = ZERO
                         MOVE   16   TO  ERR-MSG-CD
                    END-IF
                    MOVE  "R"   TO  EDIT-OPTION  OF  SIMEBI(IXA)
                    MOVE  "C"   TO  EDIT-CURSOR  OF  SIMEBI(IXA)
              END-IF
***      納品日チェック
              MOVE  "3"                TO   LINK-IN-KBN
              MOVE  NOUDAY(IXA)        TO   LINK-IN-YMD6
              MOVE  ZERO               TO   LINK-IN-YMD8
              MOVE  ZERO               TO   LINK-OUT-RET
              MOVE  ZERO               TO   LINK-OUT-YMD
              CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
              IF    LINK-OUT-RET    NOT =   ZERO
                    IF     ERR-MSG-CD   =   ZERO
                           MOVE   17   TO  ERR-MSG-CD
                    END-IF
                    MOVE  "R"   TO  EDIT-OPTION  OF  NOUDAY(IXA)
                    MOVE  "C"   TO  EDIT-CURSOR  OF  NOUDAY(IXA)
              END-IF
***      伝区
               IF    DENK (IXA)      NOT = 40 AND 41 AND 42
                     IF     ERR-MSG-CD   =  ZERO
                            MOVE   18   TO  ERR-MSG-CD
                     END-IF
                     MOVE  "R"   TO  EDIT-OPTION  OF  DENK  (IXA)
                     MOVE  "C"   TO  EDIT-CURSOR  OF  DENK  (IXA)
               END-IF
         END-IF
     END-PERFORM.
*
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     前頁処理                                  *
*----------------------------------------------------------*
 BEFORE-PAGE-SUB     SECTION.
*
     PERFORM  SETGK-START3-SUB.
     IF       SEI-FLG   =    ZERO
              PERFORM   SETGK-READ-SUB
              IF   SEI-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      8    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   BEFORE-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      8    TO   ERR-MSG-CD
              END-IF
              GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-BKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  8  BY  -1
              UNTIL   ( IXA       =    0 )  OR
                      ( SEI-FLG   =    1 )
*        伝票_
         MOVE      SEI-F05        TO   DENNO (IXA)
*        締日　　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F02            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   SIMEBI(IXA)
*        納品日　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F10            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   NOUDAY(IXA)
*        伝区　　
         MOVE      SEI-F07        TO   DENK  (IXA)
*        数量
         MOVE      SEI-F09        TO   SURYO (IXA)
*        金額　　
         MOVE      SEI-F06        TO   KINGAK(IXA)
*        店舗　　
         MOVE      SEI-F03        TO   TENCD (IXA) TEN-F011
         MOVE      TORICD         TO   TEN-F52
         PERFORM   TEN-READ-SUB
         IF        INVALID-FLG    =    1
                   MOVE   ALL NC"＊"   TO   TENNM(IXA)
         ELSE
                   MOVE   TEN-F02      TO   TENNM(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE SEI-F01        TO   SAV-BKEY-TORICD
              MOVE SEI-F05        TO   SAV-BKEY-HDENNO
         END-IF
         IF        IXA  =    8
              MOVE SEI-F01        TO   SAV-NKEY-TORICD
              MOVE SEI-F05        TO   SAV-NKEY-HDENNO
         END-IF
*
         PERFORM   SETGK-READ-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     次頁処理                                  *
*----------------------------------------------------------*
 NEXT-PAGE-SUB     SECTION.
*
     PERFORM  SETGK-START2-SUB.
     IF       SEI-FLG   =    ZERO
              PERFORM   SETGK-READ-SUB
              IF   SAV-NKEY-TORICD  =  ZERO
             AND   SAV-NKEY-HDENNO  =  SPACE
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      9    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   NEXT-PAGE-EXIT
              END-IF
              IF   SEI-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      9    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   NEXT-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      9    TO   ERR-MSG-CD
              END-IF
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-NKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    8 )  OR
                      ( SEI-FLG   =    1 )
*        伝票_
         MOVE      SEI-F05        TO   DENNO (IXA)
*        締日　　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F02            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   SIMEBI(IXA)
*        納品日　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F10            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   NOUDAY(IXA)
*        伝区　　
         MOVE      SEI-F07        TO   DENK  (IXA)
*        数量
         MOVE      SEI-F09        TO   SURYO (IXA)
*        金額　　
         MOVE      SEI-F06        TO   KINGAK(IXA)
*        店舗　　
         MOVE      SEI-F03        TO   TENCD (IXA) TEN-F011
         MOVE      TORICD         TO   TEN-F52
         PERFORM   TEN-READ-SUB
         IF        INVALID-FLG    =    1
                   MOVE   ALL NC"＊"   TO   TENNM(IXA)
         ELSE
                   MOVE   TEN-F02      TO   TENNM(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE SEI-F01        TO   SAV-BKEY-TORICD
              MOVE SEI-F05        TO   SAV-BKEY-HDENNO
         END-IF
         IF        IXA  =    8
              MOVE SEI-F01        TO   SAV-NKEY-TORICD
              MOVE SEI-F05        TO   SAV-NKEY-HDENNO
         END-IF
*
         PERFORM   SETGK-READ-SUB
     END-PERFORM.
*
 NEXT-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     請求合計ファイルＳＴＡＲＴ２　            *
*----------------------------------------------------------*
 SETGK-START2-SUB       SECTION.
*
     MOVE    ZERO            TO   SEI-FLG.
     MOVE    SPACE           TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE    SAV-NKEY-TORICD TO   SEI-F01.
     MOVE    SAV-NKEY-HDENNO TO   SEI-F05.
     START   SETGKFA    KEY  >    SEI-F01
                                  SEI-F05
       INVALID      KEY
             MOVE       1         TO   SEI-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   SEI-FLG
     END-START.
 SETGK-START2-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.4     請求合計ファイルＳＴＡＲＴ３              *
*----------------------------------------------------------*
 SETGK-START3-SUB       SECTION.
*
     MOVE    ZERO            TO   SEI-FLG.
     MOVE    SPACE           TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE    SAV-BKEY-TORICD TO   SEI-F01.
     MOVE    SAV-BKEY-HDENNO TO   SEI-F05.
     START   SETGKFA    KEY  <    SEI-F01
                                  SEI-F05
                        WITH      REVERSED  ORDER
       INVALID      KEY
             MOVE       1         TO   SEI-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   SEI-FLG
     END-START.
 SETGK-START3-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力                                  *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KAKU"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM   HEADDEL-SUB
                        PERFORM   BODYDEL-SUB
                        IF LINK-IN-TORICD  NOT = SPACE
                           MOVE LINK-IN-TORICD  TO   TORICD
                           MOVE TORICD          TO   TOK-F01
                           PERFORM     TOK-READ-SUB
                           IF   INVALID-FLG = 1
                        DISPLAY NC"取引先が登録されていません。"
                                                         UPON CONS
                           STOP   RUN
                           ELSE
                           MOVE   TOK-F02 TO   TORINM
                           MOVE "M" TO  EDIT-OPTION  OF  HDENNO
                           MOVE "C" TO  EDIT-CURSOR  OF  HDENNO
                           END-IF
                        END-IF
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        MOVE     "2"   TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   SETGK-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
                        IF  DENNO(1) = SPACE
                            MOVE ZERO         TO   SEI-FLG
                            MOVE SPACE        TO   SEI-REC
                            INITIALIZE             SEI-REC
                            MOVE TORICD       TO   SEI-F01
                            MOVE ZERO         TO   SEI-F05
                            START SETGKFA KEY > SEI-F01
                                                SEI-F05
                            END-START
                            PERFORM   SETGK-READ-SUB
                            MOVE SPACE TO SAV-NKEY
                            PERFORM  BODYDEL-SUB
                            PERFORM VARYING IXA FROM 1 BY 1
                            UNTIL ( IXA     >   8 )  OR
                                  ( SEI-FLG =   1 )
*        伝票_
         MOVE      SEI-F05        TO   DENNO (IXA)
*        締日　　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F02            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   SIMEBI(IXA)
*        納品日　
         MOVE  "3"                TO   LINK-IN-KBN
         MOVE  SEI-F10            TO   LINK-IN-YMD6
         MOVE  ZERO               TO   LINK-IN-YMD8
         MOVE  ZERO               TO   LINK-OUT-RET
         MOVE  ZERO               TO   LINK-OUT-YMD
         CALL  "SKYDTCKB"      USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD       TO   NOUDAY(IXA)
*        伝区　　
         MOVE      SEI-F07        TO   DENK  (IXA)
*        数量
         MOVE      SEI-F09        TO   SURYO (IXA)
*        金額　　
         MOVE      SEI-F06        TO   KINGAK(IXA)
*        店舗　　
         MOVE      SEI-F03        TO   TENCD (IXA) TEN-F011
         MOVE      TORICD         TO   TEN-F52
         PERFORM   TEN-READ-SUB
         IF        INVALID-FLG    =    1
                   MOVE   ALL NC"＊"   TO   TENNM(IXA)
         ELSE
                   MOVE   TEN-F02      TO   TENNM(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE SEI-F01        TO   SAV-BKEY-TORICD
              MOVE SEI-F05        TO   SAV-BKEY-HDENNO
         END-IF
         IF        IXA  =    8
              MOVE SEI-F01        TO   SAV-NKEY-TORICD
              MOVE SEI-F05        TO   SAV-NKEY-HDENNO
         END-IF
*
*
         PERFORM   SETGK-READ-SUB
     END-PERFORM
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   SETGK-UPD-SUB
                            PERFORM   NEXT-PAGE-SUB
                        END-IF
         WHEN   "E000"
                        PERFORM   KAKUCHK-SUB
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1     確認の入力チェック                        *
*----------------------------------------------------------*
 KAKUCHK-SUB             SECTION.
     MOVE       "M"     TO   EDIT-OPTION  OF  KAKU.
     MOVE       SPACE   TO   EDIT-CURSOR  OF  KAKU.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     PERFORM       SETGK-UPD-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE    "1"   TO  MAIN-FLG.
*パラメタ受取判別　　
     IF     LINK-IN-TORICD  NOT = SPACE
            MOVE   LINK-IN-TORICD  TO   TORICD
            MOVE   TORICD          TO   TOK-F01
            PERFORM     TOK-READ-SUB
            IF   INVALID-FLG = 1
                 DISPLAY NC"取引先が登録されていません。"
                                           UPON CONS
                 DISPLAY "LINK-IN-TORICD =" LINK-IN-TORICD
                                           UPON CONS
                 STOP   RUN
            ELSE
                 MOVE   TOK-F02  TO   TORINM
                 MOVE  "M"   TO  EDIT-OPTION  OF  HDENNO
                 MOVE  "C"   TO  EDIT-CURSOR  OF  HDENNO
            END-IF
     END-IF.
*
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  商品名称マスタ更新                       *
*----------------------------------------------------------*
 SETGK-UPD-SUB          SECTION.
*    商品名称マスタ更新
     PERFORM  VARYING   IXA       FROM    1  BY  1
              UNTIL     IXA  >    8
         IF   DENNO(IXA)     NOT =     SPACE
              PERFORM   SEI-READ-SUB
              IF   INVALID-FLG    =    1
                   CONTINUE
              ELSE
                   IF ( SIMEBI(IXA)    NOT =     SEI-F02 )
                        MOVE SIMEBI(IXA)    TO   SEI-F02
                   END-IF
                   IF ( NOUDAY(IXA)    NOT =     SEI-F10 )
                        MOVE NOUDAY(IXA)    TO   SEI-F10
                   END-IF
                   IF ( DENK  (IXA)    NOT =     SEI-F07 )
                        MOVE DENK  (IXA)    TO   SEI-F07
                   END-IF
                   IF ( SURYO (IXA)    NOT =     SEI-F09 )
                        MOVE SURYO (IXA)    TO   SEI-F09
                   END-IF
                   IF ( KINGAK(IXA)    NOT =     SEI-F06 )
                        MOVE KINGAK(IXA)    TO   SEI-F06
                   END-IF
                   REWRITE   SEI-REC
              END-IF
         END-IF
     END-PERFORM.
 SETGK-UPD-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      SETGKFA
              HTOKMS    HTENMS.
 END-END.
     EXIT.
************************************************************
*      9.0        共通処理                                 *
************************************************************
*==========================================================*
*      9.1       エラー メッセージセット                   *
*==========================================================*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE      SPACE     TO   ERRMSG
     ELSE
         MOVE      ERR-MSG(ERR-MSG-CD)      TO   ERRMSG
         MOVE      ZERO      TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*==========================================================*
*      9.2       項目制御部初期化                          *
*==========================================================*
*----------------------------------------------------------*
*      9.2.1     項目制御部初期化（ヘッド部）              *
*----------------------------------------------------------*
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  TORICD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TORICD.
     MOVE   "M"     TO   EDIT-OPTION  OF  TORINM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TORINM.
     MOVE   "M"     TO   EDIT-OPTION  OF  HDENNO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HDENNO.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     8
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
*
     MOVE   "M"     TO   EDIT-OPTION  OF  DENNO (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DENNO (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SIMEBI(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIMEBI(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  NOUDAY(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NOUDAY(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  DENK  (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DENK  (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SURYO (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SURYO (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  KINGAK(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KINGAK(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  TENCD (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TENCD (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  TENNM (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TENNM (IXB).
*
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
     MOVE    "FSS00101"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     WRITE    FSS00101.
 DSP-WRITE-END.
     EXIT.
*==========================================================*
*      9.4       画面データの入力処理                      *
*==========================================================*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*==========================================================*
*      9.5       ＨＥＡＤ部消去                            *
*==========================================================*
 HEADDEL-SUB            SECTION.
     MOVE   SPACE       TO   MAS003.
     PERFORM            EDIT-SET-HEAD.
 HEADDEL-END.
     EXIT.
*==========================================================*
*      9.6       ＢＯＤＹ部消去                            *
*==========================================================*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   MAS002.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.1     取引先マスタＲＥＡＤ                      *
*----------------------------------------------------------*
 TOK-READ-SUB           SECTION.
     READ    HTOKMS
       INVALID      KEY
          MOVE      1        TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
 TOK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3     請求合計データの検索                      *
*----------------------------------------------------------*
 SEI-READ-SUB           SECTION.
     MOVE    TORICD          TO   SEI-F01.
     MOVE    DENNO (IXA)     TO   SEI-F05.
     READ    SETGKFA
       INVALID      KEY
          MOVE      1        TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SEI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.4     店舗マスタ　読込　   　　                 *
*----------------------------------------------------------*
 TEN-READ-SUB           SECTION.
     READ    HTENMS
       INVALID      KEY
          MOVE      1        TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TEN-READ-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
