****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　基幹システム改善　　　　　　　　　*
*    モジュール名　　　　：　商品品単別受注状況照会　　　　　　*
*    作成日／更新日　　　：　05/12/20                          *
*    作成者／更新者　　　：　NAV MATSUN0                       *
*    処理概要　　　　　　：　受注残トランを照会する　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0500I.
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
*---<<  受注残トラン  >>---*
     SELECT   JHTZANF   ASSIGN    TO        DA-01-VI-JHTZANL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   ZAN-F07  ZAN-F091
                                                 ZAN-F092 ZAN-F01
                                                 ZAN-F06  ZAN-F02
                                                 ZAN-F04  ZAN-F051
                                                 ZAN-F03
                        FILE      STATUS    IS   ZAN-STATUS.
*
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS   ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS   ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS   ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FZA05001  OF        XMDLIB.
*---<<  発注残トラン  >>---*
 FD  JHTZANF.
     COPY     JHTZANF   OF        XFDLIB
              JOINING   ZAN       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS   OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS   OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*
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
     02  ZAN-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  EOF-FLG             PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
****  固定値  ****
 01  CONS-GYO                PIC  9(02)  VALUE 7.
****  インデックス  ****
 01  INDEX-AREA.
     02  IX1                 PIC  9(03).
     02  WK-ANS              PIC  9(01).
****  システム日付  ****
** 日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE2           PIC  9(08).
** 画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY       PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD         PIC  9(02)  VALUE  ZERO.
** 画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS         PIC  9(02)  VALUE  ZERO.
*
****  チェック用   ****
 01  CHK-YMD                 PIC  9(08).
****  ページ数   ****
 01  WK-PAGE                 PIC  9(04).
****  先頭行レコード退避   ****
     COPY     JHTZANF   OF        XFDLIB
              JOINING   ZENPG     PREFIX.
****  最終行レコード退避   ****
     COPY     JHTZANF   OF        XFDLIB
              JOINING   JIPG      PREFIX.
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0500I".
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
     02  MSG-ERR1            PIC  N(30)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(30)  VALUE
            NC"入力項目に間違いがあります。".
     02  MSG-ERR3            PIC  N(30)  VALUE
            NC"出荷場所を入力してください。".
     02  MSG-ERR4            PIC  N(30)  VALUE
            NC"倉庫マスタに存在しません。".
     02  MSG-ERR5            PIC  N(30)  VALUE
            NC"商品ＣＤを入力して下さい。".
     02  MSG-ERR6            PIC  N(30)  VALUE
            NC"対象レコードがありません。".
     02  MSG-ERR7            PIC  N(30)  VALUE
             NC"行番号を入力して下さい。".
     02  MSG-ERR8            PIC  N(30)  VALUE
             NC"行番号に誤りがあります。".
     02  MSG-ERR9            PIC  N(30)  VALUE
             NC"大小関係に誤りがあります。".
     02  MSG-ERR10           PIC  N(30)  VALUE
             NC"該当データがありません。".
     02  MSG-ERR11           PIC  N(30)  VALUE
             NC"前ページがありません。".
     02  MSG-ERR12           PIC  N(30)  VALUE
             NC"次ページがありません。".
     02  MSG-ERR13           PIC  N(30)  VALUE
             NC"１または空白を入力して下さい。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(30)
                             OCCURS 13  TIMES.
*
** 日付変換サブルーチン用領域
 01  SKYDTCKB-IN-KBN         PIC X(01).
 01  SKYDTCKB-IN-YMD6        PIC 9(06).
 01  SKYDTCKB-IN-YMD8        PIC 9(08).
 01  SKYDTCKB-OUT-RET        PIC X(01).
 01  SKYDTCKB-OUT-YMD        PIC 9(08).
** 品単ワーク
 01  WK-HINTAN.
     03  WK-HINTAN1      PIC X(05).
     03  WK-HINTAN2      PIC X(02).
     03  WK-HINTAN3      PIC X(01).
*共有フラグ
 01  EX-MAIN-AREA               IS  EXTERNAL.
     03  EX-MAIN-FLG         PIC 9(02).
*条件入力共有エリア
 01  JYOKEN-AREA             IS  EXTERNAL.
     03  JYOKEN-SOUSAI       PIC 9(01).
     03  JYOKEN-TORICD       PIC 9(08).
     03  JYOKEN-DENNO        PIC 9(09).
     03  JYOKEN-DENK         PIC 9(02).
*共有エリア用ワーク
 01  WK-JYOKEN.
     02  WK-JYOKEN1          OCCURS  7.
         03  WK-SOUSAI       PIC 9(01).
         03  WK-TORICD       PIC 9(08).
         03  WK-DENNO        PIC 9(09).
         03  WK-DENK         PIC 9(02).
*
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHTZANF.
     MOVE     "JHTZANF"        TO   ERR-FL-ID.
     MOVE      ZAN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE     "HTOKMS"        TO   ERR-FL-ID.
     MOVE      TOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE     "HTENMS"        TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS"        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS.
     MOVE     "ZSOKMS "        TO   ERR-FL-ID.
     MOVE      SOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
*----------------------------------------------------------*
 END     DECLARATIVES.
*----------------------------------------------------------*
*----------------------------------------------------------*
*      1.1       コントロール                              *
*----------------------------------------------------------*
 SZA0500I-START         SECTION.
** 終了フラグ初期化
     MOVE         SPACE         TO    END-FLG.
     PERFORM      INIT-SEC.
*    PERFORM      MAIN-SEC
*                 UNTIL     END-FLG  =    "END".
     PERFORM      MAIN-SEC
                  UNTIL   EX-MAIN-FLG =  99.
     PERFORM      END-SEC.
*     STOP      RUN.
 SZA0500I-END.
     EXIT PROGRAM.
*----------------------------------------------------------*
*      2.1       初期処理                                  *
*----------------------------------------------------------*
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF.
     OPEN     INPUT     HTENMS    HMEIMS
                        HTOKMS    ZSOKMS
                        JHTZANF.
*
** システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   SKYDTCKB-IN-KBN.
     MOVE     WK-DATE             TO   SKYDTCKB-IN-YMD6.
     MOVE     ZERO                TO   SKYDTCKB-IN-YMD8.
     MOVE     ZERO                TO   SKYDTCKB-OUT-RET.
     MOVE     ZERO                TO   SKYDTCKB-OUT-YMD.
     CALL     "SKYDTCKB"       USING   SKYDTCKB-IN-KBN
                                       SKYDTCKB-IN-YMD6
                                       SKYDTCKB-IN-YMD8
                                       SKYDTCKB-OUT-RET
                                       SKYDTCKB-OUT-YMD.
     MOVE     SKYDTCKB-OUT-YMD    TO   DATE-AREA.
*
** 画面表示日付編集
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*
** システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
** 画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
*    MOVE    "0"             TO   MAIN-FLG.
     MOVE     0              TO   EX-MAIN-FLG.
 INIT-END.
     EXIT.
*----------------------------------------------------------*
*      2.2       メイン処理                                *
*----------------------------------------------------------*
 MAIN-SEC          SECTION.
     EVALUATE      EX-MAIN-FLG
         WHEN       0   PERFORM   DSP-INIT-SUB
         WHEN       1   PERFORM   HEAD-SUB
         WHEN       2   PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.3       終了処理                                  *
*----------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE    DSPF
              JHTZANF
              HMEIMS    ZSOKMS
              HTOKMS    HTENMS.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      3.1       初期画面表示              MAIN-FLG=0      *
*----------------------------------------------------------*
 DSP-INIT-SUB         SECTION.
*
** 初期化
     MOVE     0              TO   ERR-MSG-CD.
     MOVE     1              TO   WK-PAGE.
     MOVE     SPACE          TO   FZA05001.
*
** ＰＦＭＳＧセット
     MOVE     PMSG02         TO   PFMSG.
*
*ヘッダ日時セット・画面全体更新
     PERFORM         DSP-WRITE-SUB.
     MOVE     "M"    TO   EDIT-OPTION   OF   SYUKBA.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   SYUKBA.
     MOVE     "M"    TO   EDIT-OPTION   OF   SYOCD.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   SYOCD.
     MOVE     "M"    TO   EDIT-OPTION   OF   HINTN1.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   HINTN1.
     MOVE     "M"    TO   EDIT-OPTION   OF   HINTN2.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   HINTN2.
     MOVE     "M"    TO   EDIT-OPTION   OF   HINTN3.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   HINTN3.
     MOVE     "M"    TO   EDIT-OPTION   OF   ANS.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   ANS.
*
     MOVE     1                 TO   EX-MAIN-FLG.
*
 DSP-INIT-END.
     EXIT.
*----------------------------------------------------------*
*      3.2       ヘッダ入力　　　MAIN-FLG = 1              *
*----------------------------------------------------------*
 HEAD-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM       DSP-WRITE-SUB.
*
     MOVE     SPACE          TO   ANS.
     MOVE    "HEAD"          TO   DSP-GROUP.
     PERFORM  DSP-RD-SEC.
*
** アテンション判定
     EVALUATE  DSP-FUNC
*        取消
         WHEN   "F004"
                        MOVE    0     TO   EX-MAIN-FLG
*        終了
         WHEN   "F005"
*                       MOVE   "END"  TO   END-FLG
                        MOVE    99    TO   EX-MAIN-FLG
*        実行
         WHEN   "E000"
**ヘッダチェック
                        PERFORM   HEAD-CHECK-SUB
                        IF   ERR-MSG-CD  = ZERO
                             MOVE  2  TO   EX-MAIN-FLG
**明細セット
                             MOVE     SPACE  TO  WK-JYOKEN
                             INITIALIZE          WK-JYOKEN
                             PERFORM  MEISAI-SET-SUB
                             VARYING  IX1  FROM  1  BY  1
                             UNTIL  IX1  >   CONS-GYO
**項目初期化
                             MOVE "M"   TO EDIT-OPTION OF SYUKBA
                             MOVE SPACE TO EDIT-CURSOR OF SYUKBA
                             MOVE "M"   TO EDIT-OPTION OF SYOCD
                             MOVE SPACE TO EDIT-CURSOR OF SYOCD
                             MOVE "M"   TO EDIT-OPTION OF HINTN1
                             MOVE SPACE TO EDIT-CURSOR OF HINTN1
                             MOVE "M"   TO EDIT-OPTION OF HINTN2
                             MOVE SPACE TO EDIT-CURSOR OF HINTN2
                             MOVE "M"   TO EDIT-OPTION OF HINTN3
                             MOVE SPACE TO EDIT-CURSOR OF HINTN3
                             MOVE "M"   TO EDIT-OPTION OF ANS
                             MOVE SPACE TO EDIT-CURSOR OF ANS
                        END-IF
*        その他
         WHEN    OTHER
                        MOVE    01    TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      3.2.1       ヘッダチェック　　                      *
*----------------------------------------------------------*
 HEAD-CHECK-SUB          SECTION.
*項目初期化
     MOVE     ZERO   TO   ERR-MSG-CD.
     MOVE     "M"    TO   EDIT-OPTION   OF   SYUKBA.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   SYUKBA.
     MOVE     "M"    TO   EDIT-OPTION   OF   SYOCD.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   SYOCD.
     MOVE     "M"    TO   EDIT-OPTION   OF   HINTN1.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   HINTN1.
     MOVE     "M"    TO   EDIT-OPTION   OF   HINTN2.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   HINTN2.
     MOVE     "M"    TO   EDIT-OPTION   OF   HINTN3.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   HINTN3.
     MOVE     "M"    TO   EDIT-OPTION   OF   ANS.
     MOVE     SPACE  TO   EDIT-CURSOR   OF   ANS.
*出荷場所チェック
**出荷場所入力チェック
     IF      SYUKBA          =    SPACE
             MOVE    03    TO   ERR-MSG-CD
             MOVE    "R"   TO   EDIT-OPTION  OF  SYUKBA
             MOVE    "C"   TO   EDIT-CURSOR  OF  SYUKBA
             GO TO HEAD-CHECK-END
     ELSE
**倉庫マスタ存在チェック
             MOVE  SYUKBA  TO   SOK-F01
             PERFORM  SOK-READ-SUB
             IF    INVALID-FLG  =   "1"
                   MOVE    04    TO   ERR-MSG-CD
                   MOVE    "R"   TO   EDIT-OPTION  OF  SYUKBA
                   MOVE    "C"   TO   EDIT-CURSOR  OF  SYUKBA
                   MOVE   ZERO   TO   INVALID-FLG
                   GO TO HEAD-CHECK-END
             ELSE
                   MOVE  SOK-F02 TO   SYUKNM
             END-IF
     END-IF.
*商品コード・品単チェック
**商品コード入力チェック
     IF      SYOCD           =    SPACE
             MOVE    05    TO   ERR-MSG-CD
             MOVE    "R"   TO   EDIT-OPTION  OF  SYOCD
             MOVE    "C"   TO   EDIT-CURSOR  OF  SYOCD
             GO TO HEAD-CHECK-END
     ELSE
**商品名称マスタ存在チェック
             MOVE  SYOCD   TO   MEI-F011
             MOVE  HINTN1  TO   MEI-F0121  WK-HINTAN1
             MOVE  HINTN2  TO   MEI-F0122  WK-HINTAN2
             MOVE  HINTN3  TO   MEI-F0123  WK-HINTAN3
             PERFORM  MEI-READ-SUB
             IF    INVALID-FLG  =   "1"
                   MOVE   ALL NC"＊" TO  SYONM1 SYONM2
                   MOVE   ZERO       TO  INVALID-FLG
             ELSE
                   MOVE   MEI-F021  TO   SYONM1
                   MOVE   MEI-F022  TO   SYONM2
             END-IF
     END-IF.
*レコード存在チェック
     MOVE    SYUKBA          TO   ZAN-F07.
     MOVE    SYOCD           TO   ZAN-F091.
     MOVE    WK-HINTAN       TO   ZAN-F092.
     MOVE    ZERO            TO   ZAN-F01
                                  ZAN-F06.
     PERFORM ZAN-NEXTSTART-SUB.
     IF      EOF-FLG     =   ZERO
             PERFORM     ZAN-NEXT-SUB
             IF          EOF-FLG      =    1
                         MOVE    06   TO   ERR-MSG-CD
                         MOVE   "R"   TO   EDIT-OPTION  OF  SYUKBA
                         MOVE   "C"   TO   EDIT-CURSOR  OF  SYUKBA
                         MOVE   "R"   TO   EDIT-OPTION  OF  SYOCD
                         MOVE  SPACE  TO   EDIT-CURSOR  OF  SYOCD
                         MOVE   "R"   TO   EDIT-OPTION  OF  HINTN1
                         MOVE  SPACE  TO   EDIT-CURSOR  OF  HINTN1
                         MOVE   "R"   TO   EDIT-OPTION  OF  HINTN2
                         MOVE  SPACE  TO   EDIT-CURSOR  OF  HINTN2
                         MOVE   "R"   TO   EDIT-OPTION  OF  HINTN3
                         MOVE  SPACE  TO   EDIT-CURSOR  OF  HINTN3
                         GO TO HEAD-CHECK-END
             END-IF
     ELSE
             MOVE    06    TO   ERR-MSG-CD
             MOVE    "R"   TO   EDIT-OPTION  OF  SYUKBA
             MOVE    "C"   TO   EDIT-CURSOR  OF  SYUKBA
             MOVE    "R"   TO   EDIT-OPTION  OF  SYOCD
             MOVE   SPACE  TO   EDIT-CURSOR  OF  SYOCD
             MOVE    "R"   TO   EDIT-OPTION  OF  HINTN1
             MOVE   SPACE  TO   EDIT-CURSOR  OF  HINTN1
             MOVE    "R"   TO   EDIT-OPTION  OF  HINTN2
             MOVE   SPACE  TO   EDIT-CURSOR  OF  HINTN2
             MOVE    "R"   TO   EDIT-OPTION  OF  HINTN3
             MOVE   SPACE  TO   EDIT-CURSOR  OF  HINTN3
             GO TO HEAD-CHECK-END
     END-IF.
 HEAD-CHECK-END.
     EXIT.
*----------------------------------------------------------*
*      3.3       確認入力                 MAIF-FLG = 2     *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM       DSP-WRITE-SUB.
*
     MOVE     SPACE          TO   ANS.
     MOVE    "ANS   "        TO   DSP-GROUP.
     PERFORM  DSP-RD-SEC.
*
** アテンション判定
     EVALUATE  DSP-FUNC
*        伝票照会
         WHEN   "E000"
                        PERFORM   KYOYU-SUB
*        取消
         WHEN   "F004"
                        MOVE    0     TO   EX-MAIN-FLG
*        終了
         WHEN   "F005"
*                       MOVE   "END"  TO   END-FLG
                        MOVE    99    TO   EX-MAIN-FLG
*        項目戻し
         WHEN   "F006"
                        MOVE    1     TO   EX-MAIN-FLG
*        前頁
         WHEN   "F011"
                        PERFORM   ZEN-PAGE-SUB
*        次頁
         WHEN   "F012"
                        PERFORM   JI-PAGE-SUB
*        その他
         WHEN    OTHER
                        MOVE    01    TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      4.1.1     前頁処理　                                *
*----------------------------------------------------------*
 ZEN-PAGE-SUB      SECTION.
*
     IF       ERR-MSG-CD  NOT =   ZERO
          GO       TO           ZEN-PAGE-END
     END-IF.
*
     IF       WK-PAGE     =   1
          MOVE     11           TO   ERR-MSG-CD
          GO       TO           ZEN-PAGE-END
     END-IF.
*
** データ検索
     MOVE    ZENPG-REC          TO   ZAN-REC.
     PERFORM     ZAN-BACKSTART-SUB.
     IF       EOF-FLG     =   ZERO
          PERFORM     ZAN-NEXT-SUB
     END-IF.
     IF       EOF-FLG NOT =   ZERO
          MOVE     11           TO   ERR-MSG-CD
          GO       TO           ZEN-PAGE-END
     END-IF.
*
     MOVE    SPACE              TO   JIPG-REC.
     SUBTRACT   1             FROM   WK-PAGE.
*
** 画面編集
     MOVE       SPACE     TO    WK-JYOKEN.
     INITIALIZE                 WK-JYOKEN.
     PERFORM         MEISAI-SET-SUB
                VARYING  IX1  FROM  CONS-GYO  BY  -1
                  UNTIL  IX1  <   1.
 ZEN-PAGE-END.
     EXIT.
*----------------------------------------------------------*
*      4.2.1     次頁処理　                                *
*----------------------------------------------------------*
 JI-PAGE-SUB       SECTION.
     IF       ERR-MSG-CD  NOT =   ZERO
          GO       TO           JI-PAGE-END
     END-IF.
*
     IF       JIPG-REC    =   SPACE
          MOVE     12           TO   ERR-MSG-CD
          GO       TO           JI-PAGE-END
     END-IF.
*
** データ検索
     MOVE    JIPG-REC           TO   ZAN-REC.
     PERFORM     ZAN-NEXTSTART-SUB.
     IF       EOF-FLG     =   ZERO
          PERFORM     ZAN-NEXT-SUB
     END-IF.
     IF       EOF-FLG NOT =   ZERO
          MOVE     12           TO   ERR-MSG-CD
          GO       TO           JI-PAGE-END
     END-IF.
*
     MOVE    SPACE              TO   JIPG-REC.
     ADD     1                  TO   WK-PAGE.
*
** 画面編集
     MOVE       SPACE    TO   WK-JYOKEN.
     INITIALIZE               WK-JYOKEN.
     PERFORM         MEISAI-SET-SUB
                VARYING  IX1  FROM  1  BY  1
                  UNTIL  IX1  >   CONS-GYO.
 JI-PAGE-END.
     EXIT.
*----------------------------------------------------------*
*      5.1.1     画面明細部の編集　                        *
*----------------------------------------------------------*
 MEISAI-SET-SUB          SECTION.
*
     MOVE    SPACE               TO   BODY1 (IX1).
*****COMPUTE GYO   (IX1) = (WK-PAGE - 1) * CONS-GYO + IX1.
     IF       EOF-FLG   NOT =    ZERO
             GO      TO          MEISAI-SET-END
     END-IF.
** 退避
     IF       IX1       =     1
             MOVE   ZAN-REC      TO   ZENPG-REC
     END-IF.
     IF       IX1       =     CONS-GYO
             MOVE   ZAN-REC      TO   JIPG-REC
     END-IF.
** レコード退避
     MOVE    ZAN-F04             TO   WK-SOUSAI(IX1).
     MOVE    ZAN-F01             TO   WK-TORICD(IX1).
     MOVE    ZAN-F02             TO   WK-DENNO (IX1).
     MOVE    ZAN-F051            TO   WK-DENK  (IX1).
*
** 画面編集
     MOVE    ZAN-F02             TO   DENNO (IX1).
     MOVE    ZAN-F03             TO   GYONO (IX1).
     MOVE    ZAN-F093(1:3)       TO   TANA1 (IX1).
     MOVE    ZAN-F093(4:1)       TO   TANA2 (IX1).
     MOVE    ZAN-F093(5:2)       TO   TANA3 (IX1).
     MOVE    ZAN-F081            TO   HACDAY(IX1).
     MOVE    ZAN-F082            TO   NOUDAY(IX1).
     MOVE    ZAN-F10             TO   JSURYO(IX1).
     MOVE    ZAN-F14             TO   AITECD(IX1).
     MOVE    ZAN-F01             TO   TORICD(IX1).
     MOVE    ZAN-F06             TO   TENCD (IX1).
     MOVE    ZAN-F95             TO   BTDATE(IX1).
     MOVE    ZAN-F96             TO   BTTIME(IX1).
*
** 店舗マスタの検索
     MOVE    ZAN-F01             TO   TOK-F01.
     PERFORM     TOK-READ-SUB.
     IF      INVALID-FLG   =   ZERO
            MOVE    TOK-F02      TO   TORINM(IX1)
     ELSE
            MOVE    ALL NC"＊"   TO   TORINM(IX1)
     END-IF.
** 納入先店舗マスタの検索
     MOVE    ZAN-F01             TO   TEN-F52.
     MOVE    ZAN-F06             TO   TEN-F011.
     PERFORM     TEN-READ-SUB.
     IF      INVALID-FLG   =   ZERO
            MOVE    TEN-F03      TO   TENNM(IX1)
     ELSE
            MOVE    ALL NC"＊"   TO   TENNM(IX1)
     END-IF.
*
** 次データ読み込み
     PERFORM     ZAN-NEXT-SUB.
*
 MEISAI-SET-END.
     EXIT.
*----------------------------------------------------------*
*      5.2.1     伝票照会プログラム呼び出し                *
*----------------------------------------------------------*
 KYOYU-SUB          SECTION.
     IF    ANS    NOT =   "1" AND "2" AND "3" AND "4" AND "5"
                      AND "6" AND "7"
           MOVE    07    TO   ERR-MSG-CD
           MOVE    "R"   TO   EDIT-OPTION  OF  ANS
           MOVE    "C"   TO   EDIT-CURSOR  OF  ANS
           GO TO  KYOYU-EXIT
     ELSE
           MOVE    "M"   TO   EDIT-OPTION  OF  ANS
           MOVE   SPACE  TO   EDIT-CURSOR  OF  ANS
*共用エリアの格納
           MOVE     ANS         TO   WK-ANS
           IF      WK-SOUSAI(WK-ANS)       =   ZERO
          AND      WK-TORICD(WK-ANS)       =   ZERO
          AND      WK-DENNO (WK-ANS)       =   ZERO
          AND      WK-DENK  (WK-ANS)       =   ZERO
                   MOVE    08    TO   ERR-MSG-CD
                   MOVE    "R"   TO   EDIT-OPTION  OF  ANS
                   MOVE    "C"   TO   EDIT-CURSOR  OF  ANS
                   GO TO  KYOYU-EXIT
           END-IF
           MOVE  WK-SOUSAI(WK-ANS) TO   JYOKEN-SOUSAI
           MOVE  WK-TORICD(WK-ANS) TO   JYOKEN-TORICD
           MOVE  WK-DENNO (WK-ANS) TO   JYOKEN-DENNO
           MOVE  WK-DENK  (WK-ANS) TO   JYOKEN-DENK
*          DISPLAY " JYOKEN-SOUSAI = " JYOKEN-SOUSAI UPON CONS
*          DISPLAY " JYOKEN-TORICD = " JYOKEN-TORICD UPON CONS
*          DISPLAY " JYOKEN-DENNO  = " JYOKEN-DENNO  UPON CONS
*          DISPLAY " JYOKEN-DENK   = " JYOKEN-DENK   UPON CONS
     END-IF.
*ＣＡＬＬ処理
     CALL        "SZA0505I".
*ＣＡＬＬ後処理
     MOVE    "FZA05001"           TO   DSP-FORMAT.
     MOVE    "SCREEN"             TO   DSP-GROUP.
     MOVE      "CL"               TO   DSP-PROC.
     MOVE     SPACE               TO   ANS.
     WRITE FZA05001.
 KYOYU-EXIT.
     EXIT.
*==========================================================*
*      8.0       共通処理                                  *
*==========================================================*
*----------------------------------------------------------*
*      8.1       エラー メッセージセット                   *
*----------------------------------------------------------*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE                TO   ERRMSG
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)  TO   ERRMSG
         MOVE    ZERO                 TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*----------------------------------------------------------*
*      8.2       画面表示処理                              *
*----------------------------------------------------------*
 DSP-WRITE-SUB          SECTION.
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    "FZA05001"           TO   DSP-FORMAT.
     MOVE    "SCREEN"             TO   DSP-GROUP.
     MOVE     SPACE               TO   DSP-PROC.
     MOVE     HEN-DATE            TO   SDATE.
     MOVE     HEN-TIME            TO   STIME.
     WRITE    FZA05001.
 DSP-WRITE-END.
     EXIT.
*----------------------------------------------------------*
*      8.3       画面読込処理                              *
*----------------------------------------------------------*
 DSP-RD-SEC        SECTION.
     MOVE "NE"                   TO  DSP-PROC.
     READ  DSPF.
*
 DSP-RD-EXIT.
     EXIT.
*==========================================================*
*      9.0       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.1       取引先マスタＲＥＡＤ                      *
*----------------------------------------------------------*
 TOK-READ-SUB           SECTION.
     READ    HTOKMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TOK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.2       　　　店舗マスタＲＥＡＤ                  *
*----------------------------------------------------------*
 TEN-READ-SUB           SECTION.
     READ    HTENMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TEN-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.3       商品名称マスタＲＥＡＤ                    *
*----------------------------------------------------------*
 MEI-READ-SUB           SECTION.
     READ    HMEIMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 MEI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.4       倉庫マスタ　　ＲＥＡＤ                    *
*----------------------------------------------------------*
 SOK-READ-SUB           SECTION.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SOK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.6.1.1   発注残トラン　　位置づけ（ＢＡＣＫ）　    *
*----------------------------------------------------------*
 ZAN-BACKSTART-SUB      SECTION.
     START   JHTZANF    KEY  <    ZAN-F07  ZAN-F091
                                  ZAN-F092 ZAN-F01
                                  ZAN-F06  ZAN-F02
                                  ZAN-F04  ZAN-F051
                                  ZAN-F03
                                  REVERSED
         INVALID      KEY
             MOVE      "E"        TO   EOF-FLG
         NOT INVALID  KEY
             MOVE       ZERO      TO   EOF-FLG
     END-START.
 ZAN-BACKSTART-END.
     EXIT.
*----------------------------------------------------------*
*      9.6.2.1   発注残トラン　　位置づけ（ＮＥＸＴ）　    *
*----------------------------------------------------------*
 ZAN-NEXTSTART-SUB      SECTION.
     START   JHTZANF    KEY  >    ZAN-F07  ZAN-F091
                                  ZAN-F092 ZAN-F01
                                  ZAN-F06  ZAN-F02
                                  ZAN-F04  ZAN-F051
                                  ZAN-F03
         INVALID   KEY
             MOVE      "E"        TO   EOF-FLG
         NOT INVALID  KEY
             MOVE       ZERO      TO   EOF-FLG
     END-START.
*
 ZAN-NEXTSTART-END.
     EXIT.
*----------------------------------------------------------*
*      9.6.3.1   発注残トラン　　ＲＥＡＤ　                *
*----------------------------------------------------------*
 ZAN-NEXT-SUB           SECTION.
     READ    JHTZANF    NEXT
       AT  END
         MOVE     "E"   TO   EOF-FLG
       NOT AT  END
** 出荷場所
         IF   ZAN-F07    NOT =  SYUKBA
              MOVE     "1"   TO   EOF-FLG
              GO       TO         ZAN-NEXT-END
         END-IF
** 商品ＣＤ
         IF   ZAN-F091   NOT =  SYOCD
              MOVE     "1"   TO   EOF-FLG
              GO       TO         ZAN-NEXT-END
         END-IF
** 品単　
         IF   ZAN-F092   NOT =  WK-HINTAN
              MOVE     "1"   TO   EOF-FLG
              GO       TO         ZAN-NEXT-END
         END-IF
     END-READ.
 ZAN-NEXT-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
